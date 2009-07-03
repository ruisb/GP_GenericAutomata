\documentclass[a4wide]{article} 
\usepackage{cp0708t}
\usepackage{palatino}
\usepackage{a4wide}
\usepackage[draft]{fixme}
\usepackage{fancyvrb}
\usepackage{url}
\usepackage{enumerate}

\usepackage{amsmath, amsthm, amssymb}

\allowdisplaybreaks[3]

\theoremstyle{definition}
\newtheorem{defi}{Definition}
\newtheorem{example}{Example}
\newtheorem*{conj}{Conjecture}
\newtheorem*{prob}{Problem}
\newtheorem*{question}{Question}
\theoremstyle{plain}
\newtheorem{theo}{Theorem}
\newtheorem{prop}[theo]{Proposition}
\newtheorem{lemma}[theo]{Lemma}
\newtheorem{cor}[theo]{Corolary}
\newtheorem*{theo*}{Theorem}
\newtheorem*{prop*}{Proposition}
\newtheorem*{lemma*}{Lemma}
\newtheorem*{cor*}{Corolary}
\theoremstyle{remark}
\newtheorem*{remark}{Remark}
\newtheorem*{notation}{Notation}
\def\qed{\begin{flushright} $\Box$ \end{flushright}}

\newenvironment{prf}
                {\vspace{-2mm} \noindent {\bf Proof.}}
                {\par \nopagebreak \qed }
\newenvironment{namedprf}[1]
                {\noindent {\bf Proof (#1).}}
                {\par \nopagebreak \qed }


\def\logequiv{\Leftrightarrow}

%%\def\concat{+\hspace{-10pt}+}
%%\def\smallconcat{+\hspace{-7pt}+}


%include lhs2TeX.fmt
%options ghci
%include polycode.fmt 

%format forall = "\forall"
%format === = "\doteq"
%format ?-> = "\xrightarrow{?}"
%format fst = "\p1"
%format p1  = "\p1"
%format snd = "\p2"
%format p2  = "\p2"
%%format Left = "i_1"
%%format Right = "i_2"
%%format i1 = "i_1"
%%format i2 = "i_2"
%format >< = "\times"
%format *** = "\times"
%format |-> = "\mapsto"
%format . = "\comp"
%format -|- = "+"
%%format Either a b = a "+" b
%%format (either a b) = "\alt{" a "}{" b "}"
%%format (either (a) (b)) = "\alt{" a "}{" b "}"
%format (uncurry (f)) = "(\uncurry{" f "})"
%format (curry f) = "\curry{" f "}"
%format (const f) = "\underline{" f "}"
%%format cata a = "\cata{" a "}"
%format cata (a) = "\cata{" a "}"
%format &&&  = "\ssplit{ \_ }{ \_ }"
%format (split a b) = "\ssplit{"a"}{"b"}"
%format (split (a) (b)) = "\ssplit{"a"}{"b"}"
%format `Sum` = "+"
%format `Prod` = "\otimes"
%format  Unit = "\mathbf{1}"
%format :*: = "\times"
%format :+: = "+"
%format phi = "\varphi"
%% -- desactivados:
%%format (uncurry (f)) = "\uncurry{" f "}"
%format PSet = "\fps"

\def\N{\mathbb{N}}
\def\Z{\mathbb{Z}}
\def\fps{\mathcal{P}_{fin}}

\title{
          Generic Automata               
%\\
                Generic Programming 2008/2009 \\ Universiteit Utrecht
}

\author{
		Vali Georgescul, Rui S Barbosa, Julian Verdurmen
}

\date{3rd July 2009}
%\mydate

\begin{document}

\maketitle

\begin{abstract}
We explain two generalizations of classical automata: F-automata and Tree automata. The former identifies a abstract pattern for the shape of transitions.
The second....

We show how these extensions can be implemented in Haskell using techniques and libraries from generic programming and discuss how the existent
GP libraries can be used...

We realize that both have a coalgebraic flavour and can be 
\end{abstract}

\section{Introduction}

(speak also of applications of different automata: recognizing languages, process algebras and process representations)

Automata theory is a There are 
redefinition of some operations and notions such as bisimulation.
abstract these common patterns ... and build a haskell library using gp techniques bla bla
serve several purposes
> clearer view of what they are
> simpler definitions focusing in the essentials
> general library that can be ued consistrently in many instantiations


Starting from the classical notion of a complete deterministic automata (CDFA, although),  we consider two generalizations.
shape
input
similar coalgebraic flavour


what are the aims : also see gp in action and check how good the libraries are now for supporting development (This is also (mainly?) a geeric programming exercise..se libraries bla)

\section{Abstracting the Shape: F-(Coalgebraic) Automata}

THIS IS: abstract from the shape of the transition


\subsection{Classical Automata}
We shall start by considering some classical examples of this kind of automata.


In this presentation, we shall only consider input (reactive) automata, where the transitions are determined by 
a symbol given as input (an action from the environment). However, the approach can also be extended to output
(active) automata or IO automata, as discussed... 
 
This kind of automata have a set of states |S| (this set is not necessarily finite in general) and a set |A| of input symbols
(or actions). We shall require this latter set to be finite.
The next (and most important) ingredient is a transition function. This function is what actually defines the way a transition system
reacts to each input action. 
Let us look at its shape for some well-known classes of automata

\begin{tabular}{ll}
(complete) deterministic (CDA) & |A -> S -> S|          \\ 
partial deterministic    (PDA) & |A -> S -> Unit :+: S| \\
non-deterministic        (NDA) & |A -> S -> PSet S|     \\
\end{tabular}

In the first case, the automata will move to a new state whatever the input symbol and the current state are.
The second class allows partiality of the transition function, thus the automata may not be able to move to a new state. In \Haskell,
|Unit :+: S| would probably be written as |Maybe S|. In non deterministic automata, the result of the transition function
might be an arbitrary (finite) set of states, meaning that the automata might continue through any of those states (or all in parallel)

\subsection{Other different automata}

We now consider a not so classical example. Suppose we want to express the possibility of multiple next states (as in NDA) but where these possible transitions
have some order (the informal meaning might be some kind of preference). We shall also allow repeated end states.
This ordered multiset structure is clearly
captured by a list. So, the transition function 


\subsection{Crystallizing the pattern: a Coalgebra}

***
of course, one other reason for generalizing is focusing on the essential (what is really needed to define the generic constructions) and this also contributes
to better understanding of the problem domain
(THIS COMMENT ABOVE SHOULD GO YO THE DISCUSSION OF the use ofGP)
***

\subsection{Functors Universe}

\subsection{Operations: run}

\subsection{Operations: bissimilarity}

\subsection{Other Operations}
in the same spirit

Products: left right strecnghts, etc

Composition: considering IO automata

Possibly more


\subsection{Further examples}

maybe here some reference to strange examples to really convince that this generalization is actually good, and that
it might be useful considering this general setting besides the common cases. 

weigthed automata


\section{Abstracting the Language: Tree Automata}

The second generalization we consider has to do with the kind of input that drives an automaton.
In t, . Also, .
string languages ---> tree languages

there is also a theory about regular languages: indicate some references,

\subsection{EXAMPLE}
To motivate this section, we will present an concrete example of a automata that recognizes a certain tree language.
Let us consider a language of integer and boolean expressions.
... informal explanation
... table of symbols, and their arities 

In \Haskell, a \emph{word} of this language would be an inhabitant of the following datatype
\begin{code}
data Expr  =  Zero
	   |  Suc Expr
	   |  Plus Expr Expr

	   |  False
	   |  True
	   |  And Expr Expr
           |  Not Expr

           |  Eq  Expr Expr
	   |  IfThenElse Expr Expr Expr

\end{code}

example of tree automata for expressions bla bla

\subsection{Tree automata (definitions and stuff)}

\subsection{Coalgebras, again!}




\section{PUTTING TOGHETHER THE TWO EXTENSIONS}


\section{Conclusions}
\subsection{About Generic Programming}
(an experience report on the use of gp techniques and gp libraries

\subsection{..}


\subsection{Future Work}


also accceptance conditions, and infitnite stuff!

\end{document}









