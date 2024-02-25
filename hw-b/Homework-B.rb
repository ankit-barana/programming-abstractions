# Loki Fondeur and Ankit Barana


# "Ruby in 20 Minutes" guide - https://www.ruby-lang.org/en/documentation/quickstart/3/ - helped us learn about:
   # variable declarations
   # operators
   # functions
   # for loops
   # conditions

# All of our questions in this assignment use arrays, so first looked for array-methods here - https://docs.ruby-lang.org/en/2.3.0/Array.html

# Problem 1 - merge the given lists in order - from Homework 2
def merge(list1, list2)
    return list2 if list1.empty? 
    return list1 if list2.empty? 
    if list1.first < list2.first
        [list1.first] + merge(list1[1..-1], list2)
    else
        [list2.first] + merge(list1, list2[1..-1])
    end
end


# Problem 2 - sort the given list - from Homework 2
def sort(lst)
    if lst.length == 1
        return lst
    else
        return [lst.delete(lst.min())] + sort(lst)
    end
end


# Problem 2 - adding two vectors - from homework 3
def vec_add(lst1, lst2)
    result = []
    index1 = 0
    index2 = 0
    # until we one of the lists is empty, we kee[ adding the number]
    while !lst1.empty? && !lst2.empty?
        result = result + [lst1.delete(lst1.min()) + lst2.delete(lst2.min())]
        index1 = index1 + 1
        index2 = index2 + 1
    end
    # if any of the lists is empty, we add the one that is not empty
    if lst2.empty?
            result += lst1
        elsif lst1.empty?
            result += lst2
    end
    return result
end


# Problem 1 - replacing every a with b in a given list - from homework 4
def replace(a, b, lst)
index = -1
    for item in lst
        index += 1
        if item == a
            lst.delete_at(index) 
            lst.insert(index, b)
        end
    end
    return lst
end


# Problem 3 - returns the name of the heaviest bag - from homework 4
# accepts an array containing arrays of a bag_name and bag_weight in that order
# and returns the bag_name
def heaviest(bags)
    max = -1
    heaviest_bag = ""
    for bag in bags
        if bag[1] > max
            max = bag[1]
            heaviest_bag = bag[0]
        end
    end
    return heaviest_bag
end

bags = [["duffle", 8], ["garment-bag", 2], ["briefcase", 5], ["valise", 7], ["steamer-trunk", 65], ["steamer-trunk", 0]]
list1 = [1, 3, 5, 7, 8]
list2 = [1, 2, 4]
list3 = [2, 1, 1, 1, 5, 1]
puts "#{vec_add(list1, list2)}"
puts"#{replace(1, -1, list3)}"
puts "#{heaviest(bags)}"