puts "Hello World!"
# comments
puts "Int: #{25 + 30 / 6}" # template literals
puts "floor: #{1 / 4}"
puts "coerce to Double: #{25 + 30 / 6.0}"
puts "try weird stuff: #{4 && 3}" # 4 -> true, true && 3 -> 3
my_first_variable = "nonsense"
puts "I'm talking #{my_first_variable}."
boolean = false
puts "show boolean #{boolean}"
number = -1
puts "show number #{number}"
puts "string concatenation: " + "left" + " + " + "right"
formatter = "%{a}%{b}%{c}%{d}"
puts formatter % {a: 1, b: 2, c: 3, d: 4}
puts "escape: A\nB\\ \"C\""
puts """
List:
\t* Item1
\t* Item2
\t* Item3
"""
print "How old are you? "
age = gets.chomp # readLine :: IO string
puts "I'm #{age.to_i * 10}."
