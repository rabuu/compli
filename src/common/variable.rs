/// A cheap type to refer to variables unambiguously
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Variable(usize);

impl Variable {
    /// Random variable you can use where it doesn't matter
    pub const DONT_CARE: Variable = Variable(usize::MAX);

    /// Advance internal counter to next variable
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
