def expand_from_center(s, left, right)
  while left >= 0 && right < s.length && s[left] == s[right]
    left -= 1
    right += 1
  end
  s[(left + 1)...right]
end

def longest_palindromic_substring(s)
  abort "Usage: please provide a string that contains at least one palindrome" if s.to_s.empty?

  longest = ""

  s.length.times do |i|
    odd  = expand_from_center(s, i, i)
    even = expand_from_center(s, i, i + 1)

    longest = odd  if odd.length  > longest.length
    longest = even if even.length > longest.length
  end

  abort "Usage: please provide a string that contains at least one palindrome" if longest.length <= 1

  puts longest
end

input = ARGV.first
longest_palindromic_substring(input)