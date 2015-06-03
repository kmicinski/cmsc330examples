# Ruby Matrix interpreter for CMSC 330
# Run this with 
#     ruby matrices-06-03.rb ./file.mat
# 
# @author Kris Micinski

## Matrices
## Copied from class 1
class Matrix
  attr_accessor :mat
  
  def initialize(mat)
    @mat = mat
  end
  
  def rows
    @mat.length
  end
  
  def columns
    @mat[0].length
  end

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
  
  def add(m)
    m1 = @mat
    m2 = m.mat
    sum = Array.new(self.rows,Array.new())
    
    for i in 0...self.rows
      for j in 0...self.columns
        sum[i][j] = m1[i][j] + m2[i][j]
      end
    end
    Matrix.new(sum)
  end
end

## 
## Statements
## 
class Statement
  def executeStatement(store)
    raise :error
  end
end

class StoreStatement
  def initialize(a,matrix)
    @a = a
    @matrix = matrix
  end
  
  def executeStatement(store)
    
  end
end

class AddStatement
  attr_reader :a
  attr_reader :b
  attr_reader :to
  
  def initialize(a,b,to)
    @a = a
    @b = b
    @to = to
  end
end

class PrintStatement
  def initialize(a)
    @a = a
  end
  
  def executeStatement(store)
    puts "printing `#{@a}`"
  end
end

class MultiplyStatement
  attr_reader :a
  attr_reader :b
  attr_reader :to
  
  def initialize(a,b,to)
    @a = a
    @b = b
    @to = to
  end
end

## 
## Parsing
## 
class Parser
  attr :filename
  attr_reader :statements
  
  def initialize(filename)
    @filename = filename
    @statements = []
  end
  
  def parseMatrix(str)
    matrix = []
    str.scan(/\[((\s*\d\s*)+)\]/).each do |capture|
      columns = []
      capture[0].split.each { |column| columns << column.to_i }
      matrix << columns
    end
    matrix
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
    File.readlines(@filename).each do |line|
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
  def initialize(parser)
    @parser = parser
    @environment = {}
  end
  
  def run 
    @parser.statements.each { |x|
      puts x.inspect
    }
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
  parser.parseStatements
  Interpreter.new(parser).run
end

## 
main()


