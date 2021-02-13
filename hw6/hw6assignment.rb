# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

class MyPiece < Piece
  # class array holding all the pieces and their rotations
  All_My_Pieces = [Piece.rotations([[-1, 0], [0, 0], [-1, 1], [0, -1], [1, -1]])]

  def self.next_piece (board)
    MyPiece.new(All_My_Pieces.sample, board)
  end
end

# can derived class affect parent class's fields?
# if so how can I affect it?
# class inherits method definitions but how about fields?
class MyBoard < Board
  # your enhancements here
  def initialize (game)
    @grid = Array.new(num_rows) {Array.new(num_columns)}
    @current_block = MyPiece.next_piece(self)
    @score = 0
    @game = game
    @delay = 500
  end

  def next_piece
    @current_block = MyPiece.next_piece(self)
    @current_pos = nil
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


