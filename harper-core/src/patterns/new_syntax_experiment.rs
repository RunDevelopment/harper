use std::num::NonZeroUsize;

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
    fn matches(&self, tokens: &[Token], source: &[char]) -> Option<NonZeroUsize> {
        let mut tok_cursor = 0;

        for pat in &self.patterns {
            let match_length = pat.matches(&tokens[tok_cursor..], source)?;
            tok_cursor += match_length.get();
        }

        NonZeroUsize::new(tok_cursor)
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
    fn matches(&self, tokens: &[Token], source: &[char]) -> Option<NonZeroUsize> {
        let mut longest = 0;

        for pattern in self.patterns.iter() {
            let match_len = pattern.matches(tokens, source).map_or(0, NonZeroUsize::get);
            longest = longest.max(match_len);
        }

        NonZeroUsize::new(longest)
    }
}

struct ExactWord {
    word: CharString,
}
impl Pattern for ExactWord {
    fn matches(&self, tokens: &[Token], source: &[char]) -> Option<NonZeroUsize> {
        let tok = tokens.first()?;
        if !tok.kind.is_word() {
            return None;
        }
        if tok.span.len() != self.word.len() {
            return None;
        }

        let chars = tok.span.get_content(source);
        let eq = chars == self.word.as_slice();

        NonZeroUsize::new(if eq { 1 } else { 0 })
    }
}

pub fn exact(word: &str) -> impl Pattern {
    ExactWord {
        word: word.chars().collect(),
    }
}

#[derive(Clone, Copy)]
pub struct WordTokenPattern;
impl Pattern for WordTokenPattern {
    fn matches(&self, tokens: &[Token], _source: &[char]) -> Option<NonZeroUsize> {
        let tok = tokens.first()?;
        if !tok.kind.is_word() {
            return None;
        }
        NonZeroUsize::new(1)
    }
}

pub const ANY: AnyPattern = AnyPattern;
pub const WORD: WordTokenPattern = WordTokenPattern;
pub const WS: WhitespacePattern = WhitespacePattern;

pub mod prelude {
    pub use super::super::{Pattern, WordSet};
    pub use super::{ANY, Choice, IntoPattern, Sequence, WORD, WS, exact};

    macro_rules! seq {
        ($item:expr $(,)?) => {
            IntoPattern::into_pattern($item)
        };
        ($($item:expr),* $(,)?) => {
            Sequence::new(vec![$(IntoPattern::into_pattern_boxed($item)),*])
        };
    }
    macro_rules! choice {
        ($($item:literal),+ $(,)?) => {
            WordSet::new(&[$($item),*])
        };
        ($($item:expr),* $(,)?) => {
            Choice::new(vec![$(IntoPattern::into_pattern_boxed($item)),*])
        };
    }

    pub(crate) use {choice, seq};
}
