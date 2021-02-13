class Hello
    def initialize
        @foo = 1
        @bar = "a"
    end
    def my_first_method
        puts "Hello, world!"
        puts @foo
    end

    def self.class_method
        puts "class method"
        puts @bar
    end
end


class MyHello < Hello
    def initialize
        @foo = 2
        @bar = "b"
    end
    def self.class_method
        puts "derived class method"
        puts @bar
    end
end

Hello.class_method
MyHello.class_method