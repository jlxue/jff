# Tips on Perl Debugging

---

# Agenda

- Better coding to reduce bugs
- Logging is almost all you need
- The ultima killer: Perl Debugger
- More tricks
- Recommended Materials

---

# Better Coding To Reduce Bugs

- **DO** use `warnings` and `strict`
- use `diagnostics` module for verbose explanation on warnings
- perlcritic: http://search.cpan.org/dist/Perl-Critic/
- which perl executable are you using?
- which perl version? 32bit or 64bit? thread support? @INC?
- local::lib + cpanminus, perlbrew
- PAR, PAR::Packer
- Unit test, Test::Differences
- don't reinvent wheel, always check CPAN first
- KISS principal

---

# Logging Is Almost All You Need

- Data::Dumper
- Smart::Comments
- Log4perl
- Carp, CGI::Carp

---

# The Ultima Killer: Perl Debugger

- `perldoc perldebug`: `PERLDB_OPTS="NonStop frame=1 AutoTrace LineInfo=tperl.out" perl -d a.pl`
- Ptkdb
- DDD

---

# More Tricks

- Devel::*
   - Devel::Cover
   - Devel::NYTProf
   - Devel::SmallProf
   - Devel::Trace
   - Devel::Peek
   - Devel::Size, Devel::Size::Report
- `use re 'debugcolor'`
- AOP

---

# Recommended Materials

- "Learning Perl"
- "Modern Perl"
- "Perl Best Practices"
- "Perl Debugged"
- And, `perldoc` is your friend!

