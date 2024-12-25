#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Variable(usize);

#[derive(Debug, Default)]
pub struct VariableCounter(usize);

impl VariableCounter {
    pub fn fresh(&mut self) -> Variable {
        let x = self.0;
        self.0 += 1;
        Variable(x)
    }
}
