use crate::{CharString, Token};

use super::{AnyCapitalization, AnyPattern, Pattern, WhitespacePattern};

pub trait IntoPattern {
    type Output: Pattern + 'static;
    fn into_pattern(self) -> Self::Output;

    fn into_pattern_boxed(self) -> Box<dyn Pattern + 'static>
    where
        Self: Sized,
    {
        Box::new(self.into_pattern())
    }
}
impl<T: Pattern + 'static> IntoPattern for T {
    type Output = T;
    fn into_pattern(self) -> Self::Output {
        self
    }
}
impl IntoPattern for &str {
    type Output = AnyCapitalization;
    fn into_pattern(self) -> Self::Output {
        AnyCapitalization::of(self)
    }
}

pub struct Sequence {
    patterns: Vec<Box<dyn Pattern>>,
}
impl Sequence {
    pub fn new(patterns: Vec<Box<dyn Pattern>>) -> Self {
        Self { patterns }
    }
}
impl Pattern for Sequence {
    fn matches(&self, tokens: &[Token], source: &[char]) -> Option<usize> {
        let mut tok_cursor = 0;

        for pat in &self.patterns {
            let match_length = pat.matches(&tokens[tok_cursor..], source)?;
            tok_cursor += match_length;
        }

        Some(tok_cursor)
    }
}

pub struct Choice {
    patterns: Vec<Box<dyn Pattern>>,
}
impl Choice {
    pub fn new(patterns: Vec<Box<dyn Pattern>>) -> Self {
        Self { patterns }
    }
}
impl Pattern for Choice {
    fn matches(&self, tokens: &[Token], source: &[char]) -> Option<usize> {
        let mut longest: Option<usize> = None;

        for pattern in self.patterns.iter() {
            let Some(match_len) = pattern.matches(tokens, source) else {
                continue;
            };

            longest = Some(longest.unwrap_or(0).max(match_len));
        }

        longest
    }
}

struct ExactWord {
    word: CharString,
}
impl Pattern for ExactWord {
    fn matches(&self, tokens: &[Token], source: &[char]) -> Option<usize> {
        let tok = tokens.first()?;
        if !tok.kind.is_word() {
            return None;
        }
        if tok.span.len() != self.word.len() {
            return None;
        }

        let chars = tok.span.get_content(source);
        let eq = chars == self.word.as_slice();

        if eq { Some(1) } else { None }
    }
}
pub fn exact(word: &str) -> impl Pattern {
    ExactWord {
        word: word.chars().collect(),
    }
}

struct Not<P>(P);
impl<P: Pattern> Pattern for Not<P> {
    fn matches(&self, tokens: &[Token], source: &[char]) -> Option<usize> {
        if self.0.matches(tokens, source).is_some() {
            None
        } else {
            Some(0)
        }
    }
}
pub fn not(pattern: impl Pattern) -> impl Pattern {
    Not(pattern)
}

struct Ahead<P>(P);
impl<P: Pattern> Pattern for Ahead<P> {
    fn matches(&self, tokens: &[Token], source: &[char]) -> Option<usize> {
        if self.0.matches(tokens, source).is_some() {
            Some(0)
        } else {
            None
        }
    }
}
pub fn ahead(pattern: impl Pattern) -> impl Pattern {
    Ahead(pattern)
}

/// Matches any single token, regardless of its kind.
#[derive(Clone, Copy)]
pub struct AnyToken;
impl Pattern for AnyToken {
    fn matches(&self, tokens: &[Token], _source: &[char]) -> Option<usize> {
        if tokens.is_empty() { None } else { Some(1) }
    }
}

/// Matches any word token.
#[derive(Clone, Copy)]
pub struct AnyWord;
impl Pattern for AnyWord {
    fn matches(&self, tokens: &[Token], _source: &[char]) -> Option<usize> {
        let tok = tokens.first()?;
        if !tok.kind.is_word() {
            return None;
        }
        Some(1)
    }
}

pub const WS: WhitespacePattern = WhitespacePattern;

pub mod prelude {

    pub use super::super::{Pattern, WordSet};
    pub use super::{AnyToken, AnyWord, Choice, IntoPattern, Sequence, WS, exact};

    /// Matches a sequence of patterns.
    ///
    /// This is the same as concatenating the patterns together.
    ///
    /// ## Examples
    ///
    /// ```rust
    /// use crate::patterns::new_syntax_experiment::preluse::*;
    /// let confession = seq!["I", WS, "love", WS, "you"];
    /// ```
    macro_rules! seq {
        ($item:expr $(,)?) => {
            IntoPattern::into_pattern($item)
        };
        ($($item:expr),* $(,)?) => {
            Sequence::new(vec![$(IntoPattern::into_pattern_boxed($item)),*])
        };
    }
    /// Matches any of the given patterns.
    ///
    /// ## Examples
    ///
    /// ```rust
    /// use crate::patterns::new_syntax_experiment::preluse::*;
    /// let fav_animal = choice!["dog", "cat", seq!["black", WS, "bear"]];
    /// ```
    macro_rules! choice {
        ($($item:literal),+ $(,)?) => {
            WordSet::new(&[$($item),*])
        };
        ($($item:expr),* $(,)?) => {
            Choice::new(vec![$(IntoPattern::into_pattern_boxed($item)),*])
        };
    }

    /// An assertion that matches the given sequence of patterns, but does NOT
    /// consume any tokens.
    ///
    /// ## Examples
    ///
    /// ```rust
    /// use crate::patterns::new_syntax_experiment::preluse::*;
    /// let love = seq!["I", WS, "love", ahead![WS, "you"]];
    /// ```
    macro_rules! ahead {
        ($($item:expr),* $(,)?) => {
            crate::patterns::new_syntax_experiment::ahead(seq![$($item),*])
        };
    }
    /// An assertion that matches anything but the given sequence of patterns.
    /// No tokens are consumed.
    ///
    /// ## Examples
    ///
    /// ```rust
    /// use crate::patterns::new_syntax_experiment::preluse::*;
    /// let love_no_ego = seq!["I", WS, "love", not_ahead![WS, "myself"]];
    /// ```
    macro_rules! not_ahead {
        ($($item:expr),* $(,)?) => {
            crate::patterns::new_syntax_experiment::not(
                crate::patterns::new_syntax_experiment::ahead(
                    seq![$($item),*]
                )
            )
        };
    }

    pub(crate) use {ahead, choice, not_ahead, seq};
}
