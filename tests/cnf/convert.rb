#!/usr/bin/env ruby
# encoding: US-ASCII
# Rudimentary DIMACS CNF to Manchester converter.

lines = ARGF.map do |line|
  next if line =~ /\s*[pc]/
  line.strip
    .gsub(/\d+/, '_\0')
    .gsub(/\s+/, " OR ")
    .gsub("-", "NOT ")
    .gsub(/(d+)/, "_\1")
end

puts lines.compact
  .map { |l| "(#{l})" }
  .join(" AND \n")


