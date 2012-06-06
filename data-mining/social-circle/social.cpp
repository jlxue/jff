/**
 * Given (user, url) pairs, figure out the social circles among these
 * users.
 *
 * Usage:
 *  ./social < access.log
 *
 *  The "access.log" contains (user, url) pairs separated by "\t".
 */

#include <cstdlib>
#include <iostream>
#include <map>
#include <set>
#include <vector>

using namespace std;


typedef unsigned                UserId;
typedef unsigned                UrlId;

typedef map<string, UserId>     UserToId;
typedef map<string, UrlId>      UrlToId;

typedef map<UrlId, set<UserId>* >   UrlIdToUserIds;

/**
 * The "Social" is a vector because it requires to be sorted for
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


static void read_access_log(istream&    in,
                            UserToId&   users,
                            UrlToId&    urls,
                            UrlIdToUserIds& log)
{
    unsigned users_count = 0, urls_count = 0;

    while (in.good()) {
        /*
         * split line into (user, url) pair
         */
        string line;
        getline(in, line);

        if (line.length() == 0)
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
                              Social& social)
{
    UrlIdToUserIds::const_iterator it = log.begin();
    for (/* empty */; it != log.end(); ++it) {
        set<UrlId>* urls = new set<UrlId>();
        set<UserId>* users = new set<UserId>(*(it->second));

        urls->insert(it->first);
        social.push_back(SocialCircle(urls, users));
    }
}


static bool extend_social_circle(const Social& oldSocial,
                                 Social& newSocial)
{
    bool extended = false;

    return extended;
}


int main(int argc, char** argv)
{
    UserToId users;
    UrlToId urls;
    UrlIdToUserIds log;
    vector<Social*> socials;

    read_access_log(cin, users, urls, log);

    cerr << "Unique users: " << users.size() <<
        ", unique urls: " << urls.size() << "\n";


    Social* social = new Social();
    init_first_social(log, *social);
    socials.push_back(social);


    return EXIT_SUCCESS;
}

