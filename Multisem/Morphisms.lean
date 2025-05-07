import Init.Data.Nat
import Multisem.HeytingAlgebras
import Multisem.TemporalLogic
import Multisem.Text.Macros
import Multisem.Lexicon
import Multisem.TreeSynth
open Cat
open TreeSpecs
open multisem_fix_ns
set_option synthInstance.checkSynthOrder false

-- This is the absolute simplest morphism between lexicons
instance SynthMorphBase (P:Type u)[HeytingAlgebra P](t:ContextTree MathWord)(psem:Synth P t S)(Q:Type v)[HeytingAlgebra Q][ham:HeytingAlgebraMorphism P Q] : Synth Q t S where
  denotation := ham.morph psem.denotation
  stringRep := "(morphbase "++psem.stringRep++")"
-- Marginally more interesting; weird b/c I had to constrain the HAs to be in the same universe
instance SynthMorphADJ (T:Type u)(P:Type u)[HeytingAlgebra P](t:ContextTree MathWord)(psem:Synth P t (@ADJ T))(Q:Type u)[HeytingAlgebra Q][ham:HeytingAlgebraMorphism P Q] : Synth Q t (@ADJ T) where
  denotation := λ x => ham.morph (psem.denotation x)
  stringRep := "(morphadj "++psem.stringRep++")"
--

-- Additional spec types

def ltlspec (T : Type u) (l:ContextTree MathWord) [sem:Synth (ltl.LTLFormula T) l S] : (ltl.LTLFormula T) :=
  sem.denotation
def ctlspec (T : Type u) (l:ContextTree MathWord) [sem:Synth (ctl.CTLFormula T) l S] : (ctl.CTLFormula T) :=
  sem.denotation
def ctlstarspec (T : Type u) (l:ContextTree MathWord) [sem:Synth (ctlstar.CTLStarFormula T) l S] : (ctlstar.CTLStarFormula T) :=
  sem.denotation
