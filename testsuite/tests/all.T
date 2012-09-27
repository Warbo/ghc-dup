test('mvar', [ reqlib('ghc-dup'), only_ways(['normal','optasm','profasm','ghci'])], compile_and_run, [''])
