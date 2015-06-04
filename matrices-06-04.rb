# Ruby Matrix interpreter for CMSC 330
# Run this with 
#     ruby matrices-06-04.rb ./file.mat
# 
# @author Kris Micinski

# A representation for matrices that performs various operations on
# them, using a Ruby array of arrays as the internal representation.
class Matrix
  # Define an accessor for the internal (array of arrays)
  # representation
  attr_accessor :mat

  # @param mat [Array[Array[Fixnum]]] The array based representation
  # of matrices: 
  # 
  # [ [ 1, 2],
  #   [ 0, 1] ]
  #
  def initialize(mat)
    @mat = mat
  end
  
  def rows
    @mat.length
  end
  
  def columns
    @mat[0].length
  end

  # Add {self} to matrix {m}
  # @param m Matrix
  def add(m)
    m1 = @mat
    m2 = m.mat
    sum = Array.new(self.rows,Array.new())

    (0...(self.rows)).each do |i|
      (0...(self.columns)).each do |j|
        sum[i][j] = m1[i][j] + m2[i][j]
      end
    end
    Matrix.new(sum)
  end

  # Multiply {self} and matrix {b}
  # @param m Matrix
  def product(b)
    n = self.rows
    m = self.columns
    p = b.columns
    prod = Array.new(n,[])
    n.times do |i| 
      p.times do |j|
        sum = 0
        (0...m).each { |k| sum = sum + @mat[i][k] * b.mat[k][j] }
        prod[i][j] = sum
      end
    end
    Matrix.new(prod)
  end
end

## 
## Statements
## 

# All statements have a method, {executeStatement}, that accept an
# interpreter as its argument.  The {executeStatement} method then
# performs the necessary work to execute the statement.

class StoreStatement
       
  # Construct a "store { a } { <matrix> }" statement
  # @param targetVar [String] The variable being stored into
  # @param matrix [Matrix] The matrix being stored
  def initialize(targetVar,matrix)
    @target = targetVar
    @matrix = matrix
  end
  
  # Execute a store statement by taking the interpreter's environment,
  # and updating it to have add the key,value pair
  # {(@target => @matrix)}.
  def executeStatement(interpreter)
    interpreter.environment[@target] = @matrix
  end
end

class AddStatement
  # Construct an "add { a } { b } { c }" operation that adds matrices
  # a and b, and leaves the result in C.
  # @param operand1 [String] operand 1 variable name
  # @param operand2 [String] operand 2 variable name
  # @param resultVar [String] variable to store result
  def initialize(operand1,operand2,resultVar)
    @operand1 = operand1
    @operand2 = operand2
    @resultVar = resultVar
  end
  
  # Execute a statement by taking the interpreter's environment,
  # looking up operand 1, looking up operand 2, and leaving the result
  # in C.
  def executeStatement(interpreter)
    env = interpreter.environment
    env[@resultVar] = env[@operand1].add(env[@operand2])
  end
end

class MultiplyStatement
  # Construct a "multiply { a } { b } { c }" operation that adds matrices
  # a and b, and leaves the result in C.
  def initialize(operand1,operand2,resultVar)
    @operand1 = operand1
    @operand2 = operand2
    @resultVar = resultVar
  end

  # Similar to add...
  def executeStatement(interpreter)
    env = interpreter.environment
    env[@resultVar] = env[@operand1].product(env[@operand2])
  end
end

class PrintStatement
  # Construct a "print { a }" statement
  def initialize(varName)
    @varName = varName
  end
  
  # Turn a matrix object into a string
  # @param matrix The matrix to convert
  def matrixToString(matrix)
    string = ""
    matrix.rows.times do |i|
      string = string + "| "
      matrix.columns.times { |j| string = string + matrix.mat[i][j].to_s + " " }
      string = string + "|\n"
    end
    string
  end
  
  def executeStatement(interpreter)
    matrix = interpreter.environment[@varName]
    puts "#{@varName}:", matrixToString(matrix)
  end
end

## 
## Parsing
## 
class Parser
  # The filename being parsed
  attr :filename
  # The array of `{Print,Add,...}Statement` objects
  attr_reader :statements
  # The lines of the files
  attr_reader :lines
  
  def initialize(filename)
    @filename = filename
    @statements = []
  end

  # Take a string like " [ [ 1 0 ] [ 0 1 ] ] " and turn it into a
  # {Matrix} object.
  def parseMatrix(str)
    matrix = []
    str.scan(/\[((\s*\d\s*)+)\]/).each do |capture|
      columns = []
      capture[0].split.each { |column| columns << column.to_i }
      matrix << columns
    end
    Matrix.new(matrix)
  end
  
  def parseStatement(statement)
    case statement
    when /store\s+{\s*([a-z]+)\s*}\s*{([^}]*)}/
      var = $1
      matrix = $2
      StoreStatement.new(var,parseMatrix(matrix))
    when /add\s+{\s*([a-z]+)\s*}\s*{\s*([a-z]+)\s*}\s*{\s*([a-z]+)\s*}/
      a = $1
      b = $2
      to = $3
      AddStatement.new(a,b,to)
    when /negate\s+{\s*([a-z]+)\s*}\s*{\s*([a-z]+)\s*}/
      a = $1
      to = $2
      NegateStatement.new(a,to)
    when /multiply\s+{\s*([a-z]+)\s*}\s*{\s*([a-z]+)\s*}\s*{\s*([a-z]+)\s*}/
      a = $1
      b = $2
      to = $3
      MultiplyStatement.new(a,b,to)
    when /print\s+{\s*([a-z]+)\s*}/
      PrintStatement.new($1)
    else
      nil
    end
  end
  
  def parseStatements
    i = 0
    @lines = File.readlines(@filename)
    @lines.each do |line|
      i = i+1
      statement = parseStatement(line)
      if statement then
        statements << statement
      else 
        puts "Ignoring line #{i}: `#{line.strip}`"
      end
    end
    self
  end
end

## 
## Interpreter
##

class Interpreter
  attr_accessor :environment
  
  # Construct an interpreter object
  def initialize(parser)
    @parser = parser
    @environment = {}
  end
  
  def run
    # First, parse all of the statements
    @parser.parseStatements
    line = 0
    # Next, execute each in turn
    @parser.statements.each do |statement|
      line = line+1
      # Attempt to execute the statement
      begin
        statement.executeStatement(self)
        # For debugging
        # puts "#{@parser.lines[line-1]}"        
        # @environment.each { |k,v| puts "#{k.inspect} #{v.inspect}"}
      rescue
        # If failed, tell user which line caused the problem
        puts "Error in #{@parser.filename} on line #{line}:",
             "#{@parser.lines[line-1]}"
      end
    end
  end
end

##
## Main method
##
def main
  if (ARGV.length < 1) then
    puts "ruby matrices.rb filename"
    return
  end
  filename = ARGV[0]
  parser = Parser.new(filename)
  Interpreter.new(parser).run
end

## 
main()


