use crate::{CharString, Token};

use super::SinlgeTokenPattern;

/// A [`Pattern`] that matches any capitalization of a provided word.
#[derive(Clone)]
pub struct AnyCapitalization {
    word: CharString,
}

impl AnyCapitalization {
    pub fn new(word: CharString) -> Self {
        Self { word }
    }

    pub fn of(word: &str) -> Self {
        let chars = word.chars().collect();

        Self::new(chars)
    }
}

impl SinlgeTokenPattern for AnyCapitalization {
    fn matches_token(&self, tok: &Token, source: &[char]) -> bool {
        if !tok.kind.is_word() {
            return false;
        }

        if tok.span.len() != self.word.len() {
            return false;
        }

        let tok_chars = tok.span.get_content(source);

        tok_chars
            .iter()
            .zip(&self.word)
            .all(|(a, b)| a.eq_ignore_ascii_case(b))
    }
}
