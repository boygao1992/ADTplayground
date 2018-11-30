
puts "Hello World!"
# comments
puts "Int: #{25 + 30 / 6}" # template literals
puts "floor: #{1 / 4}"
puts "coerce to Double: #{25 + 30 / 6.0}"

my_first_variable = "nonsense"
puts "I'm talking #{my_first_variable}."

boolean = false
puts "show boolean #{boolean}"

number = -1
puts "show number #{number}"

puts "string concatenation: " + "left" + " + " + "right"

formatter = "%{a}%{b}%{c}%{d}"
puts formatter % {a: 1, b: 2, c: 3, d: 4}
puts "%5s %d %10.2f" % ["one", 2, 3.33333]
# 5 = padding to 5 chars, s = String
# d = Digit
# 10 = padding to 10 chars, 3 = 3 digits after the decimal point, f = Float

puts "escape: A\nB\\ \"C\""

puts """
List:
\t* Item1
\t* Item2
\t* Item3
"""

puts <<END
List:
\t* Item1
\t* Item2
\t* Item3
END

puts "How old are you? "
prompt = "> "
print prompt
age = $stdin.gets.chomp # readLine :: IO string
puts "I'm #{age.to_i * 10}."

falsy = nil
puts "show nil: #{falsy}"
puts "falsy with default: #{falsy ||= "truthy"}"
puts "truthy && something: #{4 && 3}" # 4 -> true, true && 3 -> 3
puts "falsy || something: #{nil || 3}"

filename = ARGV.first
puts "first commandline argument: #{filename}"
txt = File.open(filename, 'r+')
=begin
multiline comments

http://ruby-doc.org/core-2.5.3/IO.html#method-c-new

Mode |  Meaning
-----+--------------------------------------------------------
"r"  |  Read-only, starts at beginning of file  (default mode).
-----+--------------------------------------------------------
"r+" |  Read-write, starts at beginning of file.
-----+--------------------------------------------------------
"w"  |  Write-only, truncates existing file
     |  to zero length or creates a new file for writing.
-----+--------------------------------------------------------
"w+" |  Read-write, truncates existing file to zero length
     |  or creates a new file for reading and writing.
-----+--------------------------------------------------------
"a"  |  Write-only, starts at end of file if file exists,
     |  otherwise creates a new file for writing.
-----+--------------------------------------------------------
"a+" |  Read-write, starts at end of file if file exists,
     |  otherwise creates a new file for reading and
     |  writing.
-----+--------------------------------------------------------
"b"  |  Binary file mode (may appear with
     |  any of the key letters listed above).
     |  Suppresses EOL <-> CRLF conversion on Windows. And
     |  sets external encoding to ASCII-8BIT unless explicitly
     |  specified.
-----+--------------------------------------------------------
"t"  |  Text file mode (may appear with
     |  any of the key letters listed above except "b").

=end
first_line = txt.readlines[0]
puts "content of #{filename}: #{first_line}"
# txt.truncate(0) # erase content
# txt.write(first_line)
txt.close

def print_two(*args) # args :: Array (Int | String | ...)
  arg1, arg2 = *args
  puts "arg1(Int): #{arg1 + 2}, arg2(String): #{arg2}"
end
print_two(1,"2",3)
def print_one(arg1)
  puts "arg1: #{arg1}"
end
print_one(1) # more than 1 argument will cause a runetime error

dice1 = 1
dice2 = 6
dice3 = 5
if dice1 > dice2
  puts "dice1 wins"
elsif dice1 > dice3
  puts "dice1 wins"
elsif dice2 > dice3
  puts "dice2 wins"
else
  puts "dice3 wins"
end

arr = [1,2,3,4]
arr.each do |idx|
  puts "arr.each: #{idx}"
end

for i in arr
  puts "for i in arr: #{i}"
end

(0..2).each do |idx| # (0..2) = [0, 1, 2], different from python
  puts "0..2: #{idx}"
end

i = 0
while i < 2
  puts "while i < 2: #{i}"
  i += 1
end

i = 0
while true
  break if i >= 2
  puts "while i < 2 with break if: #{i}"
  i += 1
end

(0..5).each do |idx|
  next if idx == 2
  puts "skip 2: #{idx}"
end
