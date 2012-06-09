/**
 * Given (user, url) pairs, figure out the social circles among these
 * users.
 *
 * Usage:
 *  ./social < access.log
 *
 *  The "access.log" contains (user, url) pairs separated by "\t".
 */

#include <algorithm>
#include <cassert>
#include <cstdlib>
#include <iostream>
#include <map>
#include <queue>
#include <set>
#include <vector>
#include <boost/shared_ptr.hpp>
#include <boost/make_shared.hpp>
#include <boost/timer/timer.hpp>

using namespace std;
using namespace boost;


typedef unsigned                UserId;
typedef unsigned                UrlId;

typedef map<string, UserId>     UserToId;
typedef map<string, UrlId>      UrlToId;

typedef map<UrlId, set<UserId>* >   UrlIdToUserIds;

/**
 * The "Social" is a vector because it requires to be ordered for
 * extend_social_circle().
 *
 *  social with small circles:
 *      [ <1> => <users...>, <2> => <users...>, ... ]
 *
 *  social with big circles:
 *      [ <1,2> => <users...>,
 *        <1,3> => <users...>,
 *        ....
 *        <2,3> => <users...>,
 *        <2,4> => <users...>,
 *        ....
 *      ]
 */
typedef pair<set<UrlId>*, set<UserId>* > SocialCircle;
typedef vector<SocialCircle> Social;


class SocialCircleUsersComparator
{
public:
    bool operator()(const SocialCircle& a,
                    const SocialCircle& b) const {
        return a.second->size() > b.second->size();
    }
};


template<typename T, typename Comparator = greater<T> >
class TopN {
public:
    TopN(unsigned n) {
        assert(n > 0);

        this->n = n;
        v.reserve(n);
    }

    T push(const T& e) {
        if (v.size() < n) {
            v.push_back(e);
            return T();
        }

        sort();

        T smallest = v[n - 1];
        if (Comparator()(e, smallest)) {
            v[n - 1] = e;
            return smallest;
        } else {
            return e;
        }
    }

    void sort() {
        std::sort(v.begin(), v.end(), Comparator());
    }

    const vector<T>& content(bool doSort = true) {
        if (doSort)
            sort();

        return v;
    }

private:
    unsigned    n;
    vector<T>   v;
};


static void read_access_log(istream&    in,
                            UserToId&   users,
                            UrlToId&    urls,
                            UrlIdToUserIds& log)
{
    boost::timer::auto_cpu_timer t;

    unsigned users_count = 0, urls_count = 0;

    while (in.good()) {
        /*
         * split line into (user, url) pair
         */
        string line;
        getline(in, line);

        if (line.empty())
            continue;

        string::size_type pos = line.find('\t');
        if (pos == string::npos ||
                pos == 0 || (pos + 1) == line.length())
            continue;

        string user = line.substr(0, pos);
        string url = line.substr(pos + 1);


        UserId user_id;
        UrlId url_id;

        /*
         * find id of this user
         */
        {
            UserToId::iterator it = users.find(user);
            if (it != users.end()) {
                user_id = it->second;
            } else {
                user_id = users_count;
                users[user] = user_id;
                ++users_count;  // XXX: abort on overflow
            }
        }


        /*
         * find id of this url
         */
        {
            UrlToId::iterator it = urls.find(url);
            if (it != urls.end()) {
                url_id = it->second;
            } else {
                url_id = urls_count;
                urls[url] = url_id;
                ++urls_count;   // XXX: abort on overflow
            }
        }

        /*
         * record this access: url_id => { user_id, ... }
         */
        {
            UrlIdToUserIds::iterator it = log.find(url_id);
            if (it != log.end()) {
                set<UserId>* users = it->second;
                users->insert(user_id);
            } else {
                set<UserId>* users = new set<UserId>();
                users->insert(user_id);
                log[url_id] = users;
            }
        }
    }
}


static void init_first_social(const UrlIdToUserIds& log,
                              Social& social,
                              unsigned maxCircleCount,
                              unsigned minUserCount)
{
    boost::timer::auto_cpu_timer t;

    if (maxCircleCount == 0)
        return;

    TopN<SocialCircle, SocialCircleUsersComparator> topN(maxCircleCount);

    UrlIdToUserIds::const_iterator it = log.begin();
    for (/* empty */; it != log.end(); ++it) {
        if (it->second->size() >= minUserCount) {
            set<UrlId>* urls = new set<UrlId>();
            set<UserId>* users = new set<UserId>(*(it->second));

            urls->insert(it->first);

            SocialCircle c(urls, users);
            SocialCircle smallest = topN.push(c);
            if (smallest.second) {
                if (smallest.second->size() == users->size()) {
                    if (false) {
                        cerr << "WARN: " << urls->size() <<
                            " degree circle drops same size(" <<
                            users->size() << ") circle due to limit " <<
                            maxCircleCount << "\n";
                    }
                }
                delete smallest.first;
                delete smallest.second;
            }
        }
    }

    const Social& v = topN.content();
    copy(v.begin(), v.end(), back_inserter(social));
}


/**
 * Fill a new social instance by extending social circles
 * in old social instance to include one more url id for
 * each circle, see the comment for "Social" type definition.
 */
static void extend_social_circle(const Social& oldSocial,
                                 Social& newSocial,
                                 unsigned maxCircleCount)
{
    boost::timer::auto_cpu_timer t;

    if (maxCircleCount == 0)
        return;

    TopN<SocialCircle, SocialCircleUsersComparator> topN(maxCircleCount);

    Social::size_type oldCircleCount = oldSocial.size();
    if (oldCircleCount < 2) {
        // need at least two circles to make a bigger circle
        return;
    }

    for (Social::size_type i = 0; i < oldCircleCount - 1; ++i) {
        SocialCircle circleA = oldSocial[i];

        for (Social::size_type j = i + 1; j < oldCircleCount; ++j) {
            SocialCircle circleB = oldSocial[j];

            set<UserId>* users = new set<UserId>();
            set_intersection(circleA.second->begin(),
                             circleA.second->end(),
                             circleB.second->begin(),
                             circleB.second->end(),
                             inserter(*users, users->begin()));

            if (users->empty()) {
                delete users;
                continue;
            }

            set<UrlId>* urls = new set<UrlId>();
            set_union(circleA.first->begin(),
                      circleA.first->end(),
                      circleB.first->begin(),
                      circleB.first->end(),
                      inserter(*urls, urls->begin()));

            SocialCircle c(urls, users);
            SocialCircle smallest = topN.push(c);
            if (smallest.second) {
                if (smallest.second->size() == users->size()) {
                    if (false) {
                        cerr << "WARN: " << urls->size() <<
                            " degree circle drops same size(" <<
                            users->size() << ") circle due to limit " <<
                            maxCircleCount << "\n";
                    }
                }

                delete smallest.first;
                delete smallest.second;
            }
        }
    }

    const Social& v = topN.content();
    copy(v.begin(), v.end(), back_inserter(newSocial));
}


int main(int argc, char** argv)
{
    (void)argc;
    (void)argv;

    boost::timer::auto_cpu_timer t;

    UserToId users;
    UrlToId urls;
    UrlIdToUserIds log;
    vector<Social*> socials;

    read_access_log(cin, users, urls, log);

    cerr << "Unique users: " << users.size() <<
        ", unique urls: " << urls.size() << "\n";


    Social* social = new Social();
    init_first_social(log, *social, 1000, 10);

    if (social->empty()) {
        delete social;
        cerr << "First social has zero social circle.\n";
    } else {
        for (;;) {
            socials.push_back(social);
            cerr << "Social: urls " << (*social)[0].first->size() <<
                ", circles " << social->size() << "\n";

            Social* newSocial = new Social();
            extend_social_circle(*social, *newSocial, social->size());
            social = newSocial;

            if (social->empty()) {
                delete social;
                break;
            }
        }
    }

    return EXIT_SUCCESS;
}

