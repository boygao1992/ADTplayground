module Interface
  def self.included(base)
    base.extend Interface::ClassMethods
  end

  module ClassMethods
    def instance_method_not_implemented(base, method_name = nil)
      if method_name.nil?
        matches = caller[0].match(/in `(.+)\'/)
        method_name = matches[1]
      end

      raise "#{base.class.name} needs to implement instance method '#{method_name}' for interface #{self.name}."
    end
  end
end

class IDoubler
  include Interface

  def double
    self + self
  end

  def + other
    IDoubler.instance_method_not_implemented(self)
  end
end

class Pt < IDoubler
  attr_accessor :x, :y

  def initialize(x, y)
    @x = x
    @y = y
  end

  def + other
    return Pt.new(@x + other.x, @y + other.y)
  end
end

pt = Pt.new(1,2)
pt2 = pt.double
puts "x: #{pt2.x}, y: #{pt2.y}"
