module AbstractInterface
  class InterfaceNotImplementedError < NoMethodError
  end

  def self.included(base)
    base.extend AbstractInterface::ClassMethods
  end

  module ClassMethods
    def api_not_implemented(base, method_name = nil)
      if method_name.nil?
        groups = caller.first.match(/in \`(.+)\'/)
        method_name  = groups[1]
      end
      raise AbstractInterface::InterfaceNotImplementedError.new("#{base.class.name} needs to implement '#{method_name}' for interface #{self.name}.")
    end
  end


end

class Bicycle
  include AbstractInterface

  def change_gear(partA, partB)
    Bicycle.api_not_implemented(self)
  end

  def speed_up(increment)
    Bycycle.api_not_implemented(self)
  end

  def apply_brakes(decrement)
    puts "apply_brakes: #{decrement}"
  end

end

class AcmeBicycle < Bicycle
end

bike = AcmeBicycle.new
bike.change_gear(1,2)

