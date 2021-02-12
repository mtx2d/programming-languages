# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

class MyPiece < Piece
  # your enhancements here
  All_My_Pieces = Piece::All_Pieces.clone
  def initialize (point_array, board)
    super
    All_My_Pieces.push([[0, 0], [0, 1], [0, 2], [0, 3], [0, 4]])
    All_My_Pieces.push([[-2, 0], [-1, 0], [0, 0], [1, 0], [2, 0]])
    All_My_Pieces.push(Piece::rotations([[-1, 0], [0, 0], [-1, 1], [0, -1], [1, -1]]))
    All_My_Pieces.push([[0, 0], [0, 1], [1, 1]])
  end

  def self.next_piece (board)
    MyPiece.new(All_My_Pieces.sample, board)
  end
end

class MyBoard < Board
  # your enhancements here
  def initialize (game)
    @grid = Array.new(num_rows) {Array.new(num_columns)}
    @current_block = MyPiece.next_piece(self)
    @score = 0
    @game = game
    @delay = 500
  end
   
end

class MyTetris < Tetris
  # your enhancements here
  # creates a canvas and the board that interacts with it
  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoard.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end

  def key_bindings  
    super
    @root.bind('u', proc {
                    @board.rotate_counter_clockwise
                    @board.rotate_counter_clockwise})
  end

end


