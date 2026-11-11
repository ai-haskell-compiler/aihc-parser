{-# LANGUAGE OverloadedStrings #-}

-- |
--
-- Module      : Aihc.Parser.Internal.CheckPattern
-- Description : Reclassify Expr trees as Pattern trees
-- License     : Unlicense
--
-- Convert an expression AST into a pattern AST. This implements the
-- \"parse as expression, then reclassify\" strategy described in
-- @docs\/unified-expr-pattern-parsing.md@.
--
-- The core function 'checkPatternWith' performs a structural recursion over
-- 'Expr', converting each expression node into its pattern counterpart.
-- Expression-only constructs (if, case, do, lambda, etc.) are rejected
-- with a descriptive error message.
module Aihc.Parser.Internal.CheckPattern
  ( checkPattern,
    checkPatternWith,
  )
where

import Aihc.Parser.Internal.Common (TokParser, isConLikeName, isExtensionEnabled, liftCheck, nameToUnqualified)
import Aihc.Parser.Syntax
import Data.Maybe (isJust, isNothing)
import Data.Text (Text)

-- | Convert an expression tree into a pattern in the parser.
-- The parser fails with a diagnostic message if the expression cannot be
-- interpreted as a valid pattern.  The enabled extensions control which
-- patterns are valid.
checkPattern :: Expr -> TokParser Pattern
checkPattern expr = do
  nPlusK <- isExtensionEnabled NPlusKPatterns
  liftCheck (checkPatternWith nPlusK expr)

-- | Convert an expression tree into a pattern.
-- The 'Bool' argument is 'True' when the @NPlusKPatterns@ extension is on.
-- Returns @Left@ with a diagnostic message if the expression cannot be
-- interpreted as a valid pattern.
checkPatternWith :: Bool -> Expr -> Either Text Pattern
checkPatternWith nPlusK expr = case expr of
  EAnn ann sub -> fmap (PAnn ann) (checkPattern' sub)
  EPragma pragma _ -> checkPragmaPattern pragma
  -- Variables and constructors
  EVar name
    | nameText name == "_" -> Right PWildcard
    | isConLikeName name -> Right (PCon name [] [])
    | isJust (nameQualifier name) -> Left "unexpected qualified name in pattern"
    | otherwise -> Right (PVar (nameToUnqualified name))
  EImplicitParam {} -> Left "unexpected implicit parameter in pattern"
  ETypeSyntax form ty -> Right (PTypeSyntax form ty)
  -- Parenthesized expression
  -- When the inner expression is a view-pattern arrow (@expr -> expr@),
  -- produce @PParen (PView f pat)@ to preserve the explicit parens in
  -- the AST, matching the shape produced by the dedicated pattern parser.
  EParen inner
    | Just vp <- asViewPat nPlusK inner -> Right (PParen vp)
    | otherwise -> PParen <$> checkPattern' inner
  -- Tuple
  ETuple fl elems
    | Just tupleCon <- tupleConstructorPattern fl elems -> Right tupleCon
    | otherwise -> do
        pats <- traverse (checkTupleElement nPlusK) elems
        Right (PTuple fl pats)
  -- List
  EList elems -> PList <$> traverse checkPattern' elems
  -- Unboxed sum
  EUnboxedSum i n e -> PUnboxedSum i n <$> checkPattern' e
  -- View pattern: @expr -> pat@.
  EViewPat l r -> do
    rPat <- checkPattern' r
    Right (PView l rPat)
  -- Infix: only constructor operators (starting with ':') are valid in patterns.
  EInfix l op r
    | isConLikeOp op -> do
        lPat <- checkPattern' l
        rPat <- checkPattern' r
        Right (PInfix lPat op rPat)
    | nPlusK,
      Just pat <- nPlusKPattern l op r ->
        Right pat
    | otherwise -> Left ("unexpected variable operator '" <> renderName op <> "' in pattern")
  -- Type signature
  ETypeSig e ty -> do
    pat <- checkPattern' e
    Right (PTypeSig pat ty)
  -- Negation (must be a literal)
  ENegate inner -> checkNegLitPattern inner
  -- Application: accumulate arguments into PCon
  EApp f x -> do
    fPat <- checkPattern' f
    xPat <- checkPattern' x
    case peelPatternAnn fPat of
      PCon name typeArgs args -> Right (PCon name typeArgs (args ++ [xPat]))
      PBuiltinCon con typeArgs args -> Right (PBuiltinCon con typeArgs (args ++ [xPat]))
      _ -> Left "invalid pattern: application of non-constructor"
  -- Record construction -> record pattern
  ERecordCon name fields wc -> do
    patFields <-
      traverse
        ( \field -> do
            pat <- checkPattern' (recordFieldValue field)
            pure field {recordFieldValue = pat}
        )
        fields
    Right (PRecord name patFields wc)
  -- Literals
  EInt n nt repr -> Right (PLit (LitInt n nt repr))
  EFloat x ft repr -> Right (PLit (LitFloat x ft repr))
  EChar c repr -> Right (PLit (LitChar c repr))
  ECharHash c repr -> Right (PLit (LitCharHash c repr))
  EString s repr -> Right (PLit (LitString s repr))
  EStringHash s repr -> Right (PLit (LitStringHash s repr))
  EOverloadedLabel {} -> Left "unexpected overloaded label in pattern"
  -- TH splice
  ETHSplice body -> Right (PSplice body)
  -- Quasi-quote
  EQuasiQuote q b -> Right (PQuasiQuote q b)
  -- Expression-only constructs: clear errors
  EIf {} -> Left "unexpected if-then-else in pattern"
  EMultiWayIf {} -> Left "unexpected multi-way if in pattern"
  ECase {} -> Left "unexpected case expression in pattern"
  EDo {} -> Left "unexpected do expression in pattern"
  ELambdaPats {} -> Left "unexpected lambda in pattern"
  ELambdaCase {} -> Left "unexpected lambda-case in pattern"
  ELambdaCases {} -> Left "unexpected lambda-cases in pattern"
  ELetDecls {} -> Left "unexpected let expression in pattern"
  EArithSeq {} -> Left "unexpected arithmetic sequence in pattern"
  EListComp {} -> Left "unexpected list comprehension in pattern"
  EListCompParallel {} -> Left "unexpected parallel list comprehension in pattern"
  ESectionL {} -> Left "unexpected left section in pattern"
  ESectionR {} -> Left "unexpected right section in pattern"
  ERecordUpd {} -> Left "unexpected record update in pattern"
  EGetField {} -> Left "unexpected record field access in pattern"
  EGetFieldProjection {} -> Left "unexpected projection section in pattern"
  ETypeApp fun ty -> do
    funPat <- checkPattern' fun
    case peelPatternAnn funPat of
      PCon name typeArgs args -> Right (PCon name (typeArgs ++ [ty]) args)
      PBuiltinCon con typeArgs args -> Right (PBuiltinCon con (typeArgs ++ [ty]) args)
      _ -> Left "unexpected type application in pattern"
  ETHExpQuote {} -> Left "unexpected Template Haskell expression quote in pattern"
  ETHTypedQuote {} -> Left "unexpected Template Haskell typed quote in pattern"
  ETHDeclQuote {} -> Left "unexpected Template Haskell declaration quote in pattern"
  ETHTypeQuote {} -> Left "unexpected Template Haskell type quote in pattern"
  ETHPatQuote {} -> Left "unexpected Template Haskell pattern quote in pattern"
  ETHNameQuote {} -> Left "unexpected Template Haskell name quote in pattern"
  ETHTypeNameQuote {} -> Left "unexpected Template Haskell type name quote in pattern"
  ETHTypedSplice {} -> Left "unexpected typed Template Haskell splice in pattern"
  EProc {} -> Left "unexpected proc expression in pattern"
  where
    checkPattern' = checkPatternWith nPlusK

checkPragmaPattern :: Pragma -> Either Text Pattern
checkPragmaPattern pragma =
  case pragmaType pragma of
    PragmaSCC {} -> Left "unexpected SCC pragma in pattern"
    _ -> Left "unexpected pragma in pattern"

-- | Check that a tuple element is not a tuple section (Nothing).
checkTupleElement :: Bool -> Maybe Expr -> Either Text Pattern
checkTupleElement _ Nothing = Left "unexpected tuple section in pattern"
checkTupleElement nPlusK (Just e) = checkPatternWith nPlusK e

-- | A tuple section with no fields, such as @(,)@ or @(#,,#)@, is the
-- prefix tuple constructor.
tupleConstructorPattern :: TupleFlavor -> [Maybe Expr] -> Maybe Pattern
tupleConstructorPattern fl elems
  | null elems = Nothing
  | all isNothing elems = Just (PBuiltinCon (BuiltinTuple fl (length elems)) [] [])
  | otherwise = Nothing

-- | Check that a negated expression is a literal (for PNegLit patterns).
checkNegLitPattern :: Expr -> Either Text Pattern
checkNegLitPattern inner = case inner of
  EInt n nt repr -> Right (PNegLit (LitInt n nt repr))
  EFloat x ft repr -> Right (PNegLit (LitFloat x ft repr))
  EAnn ann sub -> fmap (PAnn ann) (checkNegLitPattern sub)
  _ -> Left "negation in pattern requires a numeric literal"

-- | Try to interpret @l op r@ as an n+k pattern such as @n+1@.
-- The operator must be an unqualified @+@, the left operand must be an
-- unqualified variable, and the right operand must be a plain integer literal.
nPlusKPattern :: Expr -> Name -> Expr -> Maybe Pattern
nPlusKPattern l op r
  | isNothing (nameQualifier op),
    nameType op == NameVarSym,
    nameText op == "+",
    Just var <- nPlusKVar l,
    Just lit <- nPlusKLiteral r =
      Just (PNPlusK var lit)
  | otherwise = Nothing
  where
    nPlusKVar e =
      case e of
        EAnn _ sub -> nPlusKVar sub
        EVar name
          | isNothing (nameQualifier name),
            nameType name == NameVarId,
            nameText name /= "_" ->
              Just (nameToUnqualified name)
        _ -> Nothing
    nPlusKLiteral e =
      case e of
        EAnn ann sub -> LitAnn ann <$> nPlusKLiteral sub
        EInt n TInteger repr -> Just (LitInt n TInteger repr)
        _ -> Nothing

-- | Check whether an operator is a constructor operator (starts with ':').
-- Constructor operators and backtick-quoted constructors are valid in patterns;
-- variable operators like @+@ or @*@ are not.
isConLikeOp :: Name -> Bool
isConLikeOp = isConLikeName

-- | Try to interpret an expression as a view pattern @expr -> expr@.
-- Returns 'Just' the corresponding 'PView' when the expression is an
-- 'EViewPat'; 'Nothing' otherwise.  Used by the 'EParen' case of
-- 'checkPattern' to strip the outer parentheses and produce @PView@
-- directly (matching the AST shape that the dedicated pattern parser
-- produces).
asViewPat :: Bool -> Expr -> Maybe Pattern
asViewPat nPlusK (EViewPat l r) =
  case checkPatternWith nPlusK r of
    Right rPat -> Just (PView l rPat)
    Left _ -> Nothing
asViewPat _ _ = Nothing
