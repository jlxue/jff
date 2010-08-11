package Util;

use Exporter 'import';

@EXPORT_OK = qw/array_as_hash_keys/;

#sub any {
    # benchmark:
    # ~~ >> for > Perl6::Junction::any > List::Util::first, List::MoreUtils::any
    # grep 在数组不大或者要搜索的元素在数组后部时效率高
    # 如果要多次搜索，构造 hash 然后查找快的多
#}

sub array_as_hash_keys {
    my %hash;

    \( @hash{@_} );     # autovivification
                        # 次快的方案： @hash{@_} = (1) x @_;

    return \%hash;
}

1;

