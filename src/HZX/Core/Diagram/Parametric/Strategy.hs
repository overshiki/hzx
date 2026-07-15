-- | Simplification strategies for parametric ZX diagrams.
module HZX.Core.Diagram.Parametric.Strategy
  ( ParamStrategy
  , pRunStrategy
  , pSpiderSimp
  , pPhaseFreeSimp
  , pBasicSimp
  , pToGraphLike
  , pCliffordSimp
  ) where

import HZX.Core.Diagram.Parametric.Types (ParamDiagram)
import HZX.Core.Diagram.Parametric.Rewrite
  ( ParamRule
  , pSpiderFusion, pIdentityRemoval, pHadamardEdgeSimp, pColorChange
  , pHopfRule, pSelfLoopRemoval, pPiCopy, pBialgebraSimp
  , pLocalComplementation, pPivot
  )
import HZX.Rewrite.Generic (fixpoint)

type ParamStrategy = ParamDiagram -> ParamDiagram

-- | Repeatedly apply a parametric rule until it no longer applies.
pRunStrategy :: ParamRule -> ParamStrategy
pRunStrategy = fixpoint

-- | Spider fusion followed by self-loop removal.
pSpiderSimp :: ParamStrategy
pSpiderSimp d = pRunStrategy pSpiderFusion (pRunStrategy pSelfLoopRemoval d)

-- | Phase-free simplification.
pPhaseFreeSimp :: ParamStrategy
pPhaseFreeSimp = pRunStrategy phaseFreeRule
  where
    phaseFreeRule d =
      case pHadamardEdgeSimp d of
        Just d' -> Just d'
        Nothing ->
          case pBialgebraSimp d of
            Just d' -> Just d'
            Nothing ->
              case pSpiderFusion d of
                Just d' -> Just d'
                Nothing ->
                  case pIdentityRemoval d of
                    Just d' -> Just d'
                    Nothing ->
                      case pHopfRule d of
                        Just d' -> Just d'
                        Nothing -> pSelfLoopRemoval d

-- | Basic simplification: iterate phase-free rules to fixpoint.
pBasicSimp :: ParamStrategy
pBasicSimp d =
  let d' = pPhaseFreeSimp d
  in if d' == d then d else pBasicSimp d'

-- | Convert a parametric diagram to graph-like form.
pToGraphLike :: ParamStrategy
pToGraphLike d =
  let d1 = pRunStrategy pHadamardEdgeSimp d
      d2 = pRunStrategy pColorChange d1
      d3 = pSpiderSimp d2
  in d3

-- | Clifford simplification for parametric diagrams.
pCliffordSimp :: ParamStrategy
pCliffordSimp d0 =
  let d1 = pToGraphLike d0
      d2 = pRunStrategy cliffordRule d1
  in d2
  where
    cliffordRule d =
      case pLocalComplementation d of
        Just d' -> Just d'
        Nothing ->
          case pPivot d of
            Just d' -> Just d'
            Nothing ->
              case pSpiderFusion d of
                Just d' -> Just d'
                Nothing ->
                  case pIdentityRemoval d of
                    Just d' -> Just d'
                    Nothing ->
                      case pPiCopy d of
                        Just d' -> Just d'
                        Nothing -> Nothing
