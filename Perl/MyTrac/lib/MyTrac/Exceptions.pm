package MyTrac::Exceptions;
use Exception::Class (
    MyTrac::Database::Exception => {
        description => 'Database related exception',
        fields      => ['id', 'operation', 'errno'],
    },
);

1;

