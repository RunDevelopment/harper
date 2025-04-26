use std::num::NonZeroUsize;

use crate::{CharString, Token};

use super::{AnyCapitalization, AnyPattern, Pattern, WhitespacePattern};

pub trait IntoPattern {
    type Output: Pattern + 'static;
    fn into_pattern(self) -> Self::Output;
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

pub trait IntoPatternList {
    fn into_pattern_list(self) -> Vec<Box<dyn Pattern>>;
}
impl IntoPatternList for Vec<Box<dyn Pattern>> {
    #[inline]
    fn into_pattern_list(self) -> Vec<Box<dyn Pattern>> {
        self
    }
}
impl IntoPatternList for () {
    #[inline]
    fn into_pattern_list(self) -> Vec<Box<dyn Pattern>> {
        Vec::new()
    }
}
impl<P: IntoPattern> IntoPatternList for P {
    #[inline]
    fn into_pattern_list(self) -> Vec<Box<dyn Pattern>> {
        vec![Box::new(self.into_pattern())]
    }
}

macro_rules! impl_into_pattern_list {
    ($($name:ident = $index:tt),+) => {
        impl<$($name: IntoPattern),+> IntoPatternList for ($($name),+) {
            #[inline]
            fn into_pattern_list(self) -> Vec<Box<dyn Pattern>> {
                vec![$(Box::new(self.$index.into_pattern())),+]
            }
        }
    };
}
impl_into_pattern_list!(A = 0, B = 1);
impl_into_pattern_list!(A = 0, B = 1, C = 2);
impl_into_pattern_list!(A = 0, B = 1, C = 2, D = 3);
impl_into_pattern_list!(A = 0, B = 1, C = 2, D = 3, E = 4);
impl_into_pattern_list!(A = 0, B = 1, C = 2, D = 3, E = 4, F = 5);
impl_into_pattern_list!(A = 0, B = 1, C = 2, D = 3, E = 4, F = 5, G = 6);
impl_into_pattern_list!(A = 0, B = 1, C = 2, D = 3, E = 4, F = 5, G = 6, H = 7);
#[rustfmt::skip]
impl_into_pattern_list!(A = 0, B = 1, C = 2, D = 3, E = 4, F = 5, G = 6, H = 7, I = 8);
#[rustfmt::skip]
impl_into_pattern_list!(A = 0, B = 1, C = 2, D = 3, E = 4, F = 5, G = 6, H = 7, I = 8, J = 9);
#[rustfmt::skip]
impl_into_pattern_list!(A = 0, B = 1, C = 2, D = 3, E = 4, F = 5, G = 6, H = 7, I = 8, J = 9, K = 10);
#[rustfmt::skip]
impl_into_pattern_list!(A = 0, B = 1, C = 2, D = 3, E = 4, F = 5, G = 6, H = 7, I = 8, J = 9, K = 10, L = 11);

pub struct Sequence {
    pub patterns: Vec<Box<dyn Pattern>>,
}
impl Sequence {
    pub fn new(patterns: impl IntoPatternList) -> Self {
        Self {
            patterns: patterns.into_pattern_list(),
        }
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
    pub patterns: Vec<Box<dyn Pattern>>,
}
impl Choice {
    pub fn new(patterns: impl IntoPatternList) -> Self {
        Self {
            patterns: patterns.into_pattern_list(),
        }
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
    pub use super::{ANY, Choice, Sequence, WORD, WS, exact};

    macro_rules! seq {
        ($($item:expr),* $(,)?) => {
            Sequence::new(($($item),*))
        };
    }
    pub(crate) use seq;
}
