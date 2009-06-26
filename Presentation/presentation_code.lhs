\documentclass[a4wide,12pt]{article} 
\usepackage{cp0708t}
\usepackage{palatino}
\usepackage{a4wide}
\usepackage[draft]{fixme}
%\allowdisplaybreaks[3]
\usepackage{fancyvrb}
\usepackage{url}
\usepackage{enumerate}
\usepackage[usenames]{color}
\usepackage{amsmath, amsthm, amssymb}


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

\def\alert#1{\color{red}#1\color{black}}

%%\def\concat{+\hspace{-10pt}+}
%%\def\smallconcat{+\hspace{-7pt}+}


%include lhs2TeX.fmt
%options ghci
%include polycode.fmt 
%format === = "\doteq"
%format ?-> = "\xrightarrow{?}"
%format fst = "\p1"
%format p1  = "\p1"
%format snd = "\p2"
%format p2  = "\p2"
%format Left = "i_1"
%format Right = "i_2"
%format i1 = "i_1"
%format i2 = "i_2"
%format >< = "\times"
%format |-> = "\mapsto"
%format . = "\comp"
%format -|- = "+"
%format Either a b = a "+" b
%format (either a b) = "\alt{" a "}{" b "}"
%format (either (a) (b)) = "\alt{" a "}{" b "}"
%format (uncurry (f)) = "(\uncurry{" f "})"
%format (curry f) = "\curry{" f "}"
%format (const f) = "\underline{" f "}"
%%format cata a = "\cata{" a "}"
%format cata (a) = "\cata{" a "}"
%format (split a b) = "\ssplit{"a"}{"b"}"
%format (split (a) (b)) = "\ssplit{"a"}{"b"}"
%format <=< = "\bullet"
%format delta = "\delta"
%format S.empty = "\emptyset"
%format S.union = "\cup"
%% -- desactivados:
%%format (uncurry (f)) = "\uncurry{" f "}"

\def\N{\mathbb{N}}



\begin{document}

\begin{code}
{-"\noindent\text{Transition System (a Coalgebra)}"-}
type TS f s     = s -> f s

{-"\noindent\text{Labelled Transition System}"-}
type LTS a f s  = Map a (TS f s)  {-"\;\;\fbox{map is a finite function}"-}

{-"~\\"-}
alphabet = M.keys

{-"~"-}

infixl 9 \$
(\$)    :: Map a b -> (a -> b)
f \$ k  = guardFromJust "action not defined" (M.lookup k f)


guardFromJust _    (Just x)  = x
guardFromJust err  Nothing   = error err
\end{code}


\newpage
\fbox{
\begin{minipage}{\textwidth}
\begin{code}
runForgetful :: (..., Crush f, Functor f) => LTS a f s -> [a] -> s -> Set s
runForgetful delta []      = {-"\alert{"-}singleton {-"}"-}  
runForgetful delta (a:as)  = {-"\alert{"-}setjoin . toSet{-"}"-} . fmap (runForgetful delta as) . (delta \$ a)
\end{code}
\end{minipage}
}

\hspace{1.5cm}
\begin{minipage}{0.7\textwidth}
\begin{code}
toSet :: (..., Crush f) => f a -> Set a
toSet = {-"\alert{"-}crush{-"}"-} S.insert S.empty 
{-"~\\"-}
setjoin :: Set (Set a) -> Set a
\end{code}
\end{minipage}

\newpage

\begin{code}
runForgetful :: (..., Crush f, Functor f) => LTS a f s -> [a] -> s -> Set s
runForgetful delta []      = {-"\alert{"-}singleton {-"}"-}  
runForgetful delta (a:as)  = {-"\alert{"-}setjoin . toSet{-"}"-} . fmap (runForgetful delta as) . (delta \$ a)
\end{code}

\fbox{
\begin{minipage}{\textwidth}
\begin{code}
runPreserving :: (..., Functor f) => LTS a f s -> [a] -> s -> Star f s
runPreserving delta []      = {-"\alert{"-}End {-"}"-}  
runPreserving delta (a:as)  = {-"\alert{"-}Step{-"}"-} . fmap (runPreserving delta as) . (delta \$ a)
\end{code}
\end{minipage}}

\hspace{1.5cm}
\begin{minipage}{0.7\textwidth}
\begin{code}
data Star f s = End s | Step (f (Star f s))
\end{code}
\end{minipage}

\newpage

\begin{code}
runForgetful :: (..., Crush f, Functor f) => LTS a f s -> [a] -> s -> Set s
runForgetful delta []      = {-"\alert{"-}singleton {-"}"-}  
runForgetful delta (a:as)  = {-"\alert{"-}setjoin . toSet{-"}"-} . fmap (runForgetful delta as) . (delta \$ a)
\end{code}

\begin{code}
runPreserving :: (..., Functor f) => LTS a f s -> [a] -> s -> Star f s
runPreserving delta []      = {-"\alert{"-}End {-"}"-}  
runPreserving delta (a:as)  = {-"\alert{"-}Step{-"}"-} . fmap (runPreserving delta as) . (delta \$ a)
\end{code}

\fbox{
\begin{minipage}{\textwidth}
\begin{code}
runInMonad :: (..., Functor f, Monad f) => LTS a f s -> [a] -> s -> f s
runInMonad delta []      = {-"\alert{"-}return{-"}"-}
runInMonad delta (a:as)  = runInMonad delta as <=< (delta \$ a)
{-"\hspace{4cm}or"-}
runInMonad delta (a:as)  = {-"\alert{"-}join{-"}"-} . runInMonad delta as . (delta \$ a)
\end{code}
\end{minipage}
}

\newpage

\begin{code}
bisimilar :: (Eq s, Ord a) => LTS a f s -> s -> s -> Bool
bisimilar delta p q = runReader (bisim delta p q) [ ]
\end{code}

\begin{code}
bisim :: (Eq s, Ord a) => LTS a f s -> s -> s -> Reader [(s,s)] Bool
bisim delta p q = do  stack <- ask 
                      if p==q || (p,q) `elem` stack || (q,p) `elem` stack 
                         then return True
                         else liftM and $ mapM (bisimBy delta p q) (alphabet delta)


bisimBy :: (Eq s, Ord a) => LTS a f s -> s -> s -> a -> Reader [(s,s)] Bool
bisimBy delta p q a =  let  p' = (delta \$ a) p
                            q' = (delta \$ a) q
                       in local ((p,q):) $
                           liftM (maybe False and) $ fSafeZipWithM (bisim delta) p' q' 
\end{code}

\alert{
\begin{code}
fSafeZipWithM :: (a -> b -> m c) -> f a -> f b -> m (Maybe (f c))
\end{code}
}


\end{document}

