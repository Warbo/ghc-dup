supported_ways = only_ways(['normal','optasm','ghci'])

test('mvar', [ supported_ways ], compile_and_run, [''])
test('enum', [ supported_ways, stats_range_field('bytes allocated',1652816,10), stats_range_field('max_bytes_used',28504,10) ], compile_and_run, [''])
