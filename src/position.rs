#[derive(Debug)]
pub struct Position {
    column: usize,
    row: usize,
}

impl Position {
    pub fn new(row: usize, column: usize) -> Self {
        Position { column, row }
    }

    pub fn next_row(&mut self) {
        self.column = 0;
        self.row += 1;
    }

    pub fn next_column(&mut self) {
        self.column += 1;
    }
}

impl Clone for Position {
    fn clone(&self) -> Self {
        Self {
            column: self.column.clone(),
            row: self.row.clone(),
        }
    }
}

impl Copy for Position {}
