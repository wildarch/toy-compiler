use rand::distributions::Standard;
use rand::prelude::*;

use names::{ADJECTIVES, NOUNS};

pub struct RandomIdent {
    noun: &'static str,
    adjective: &'static str,
}

impl ToString for RandomIdent {
    fn to_string(&self) -> String {
        format!("{}-{}", self.noun, self.adjective)
    }
}

impl Distribution<RandomIdent> for Standard {
    fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> RandomIdent {
        RandomIdent {
            noun: NOUNS.choose(rng).unwrap(),
            adjective: ADJECTIVES.choose(rng).unwrap(),
        }
    }
}
