#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Variable(usize);

impl Variable {
    pub const DONT_CARE: Variable = Variable(usize::MAX);

    pub fn advance(&mut self) {
        self.0 += 1;
    }
}

impl std::fmt::Display for Variable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if *self == Self::DONT_CARE {
            write!(f, "VAR#?")
        } else {
            write!(f, "VAR#{}", self.0)
        }
    }
}

