package MyTrac::Exceptions;
use Exception::Class (
    MyTrac::Database::Exception => {
        description => 'Database related exception',
    },

    MyTrac::Database::OperateException => {
        isa         => 'MyTrac::Database::Exception',
        description => 'Database operation related exception',
        fields      => ['id', 'operation', 'errno'],
    },
);

MyTrac::Database::Exception->Trace(1);

1;

