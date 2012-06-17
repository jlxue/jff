/**
 * Given (user, url) pairs, figure out the social circles among these
 * users.
 *
 * Usage:
 *  ./social < access.log
 *
 *  The "access.log" contains (user, url) pairs separated by spaces.
 *  DO sanitize the input, the "user" and "url" fields mustn't
 *  contain white spaces.
 */

#include <algorithm>
#include <cassert>
#include <cstdlib>
#include <iostream>
#include <limits>
#include <map>
#include <set>
#include <vector>

// GCC < 4.7 defines __cplusplus to 1 not 201103L even -std=c++0x
// is given: http://gcc.gnu.org/bugzilla/show_bug.cgi?id=1773
#if __cplusplus >= 201103L || __GXX_EXPERIMENTAL_CXX0X__
#include <cstdint>
#include <unordered_map>
#else
#include <boost/cstdint.hpp>
#include <boost/unordered_map.hpp>
#endif


#ifdef  ENABLE_CPU_TIMER

#include <boost/timer/timer.hpp>

#define DECLARE_AUTO_CPU_TIMER_WITH_NAME(name, t)   \
    boost::timer::auto_cpu_timer t(std::cerr, \
            boost::timer::default_places, \
            "\n\t\t" + string(name) + \
            ": %ws wall, %us user + %ss system = %ts CPU (%p%)\n\n")

#define DECLARE_AUTO_CPU_TIMER(t)   \
    DECLARE_AUTO_CPU_TIMER_WITH_NAME(__func__, t)

#else

#define DECLARE_AUTO_CPU_TIMER_WITH_NAME(name, t) (void)0
#define DECLARE_AUTO_CPU_TIMER(t) (void)0

#endif


using namespace std;


/*
 * 32bit integer is enough, the IDs are stored in many data structures,
 * so don't waste memory on 64bit integers.
 */
typedef uint32_t                UserId;
typedef uint32_t                UrlId;

#if __cplusplus >= 201103L || __GXX_EXPERIMENTAL_CXX0X__
typedef unordered_map<string, UserId>   UserToId;
typedef unordered_map<string, UrlId>    UrlToId;
#else
typedef boost::unordered_map<string, UserId>    UserToId;
typedef boost::unordered_map<string, UrlId>     UrlToId;
#endif

/*
 * The value must be ordered set, because init_first_social() and
 * member "users" in SocialCircle structure below depend on that.
 */
typedef map<UrlId, set<UserId>* >   AccessLogByUrl;

/*
 * Actually both urls and users are "set" type with ordered and unique
 * members, but to make urls_union() and set_intersection() in
 * extend_social_circles() more efficient on cpu and memory, "vector"
 * type is chose.
 */
struct SocialCircle {
    vector<UrlId>   urls;
    vector<UserId>  users;

    // For example, in old social, (url1, url2) is visited by (userA,
    // userB), in new social, (url1, url2, url3) is also visited by
    // (userA, userB), then this field records the index of the smaller
    // circle.
    uint32_t        subCircleIndex;

    // To avoid duplicated set intersections in extend_social_circles(),
    // can't reply on urlId because SocialCircles in a Social is ordered
    // by users.size(), not by urlId.
    //
    // Only for internal implementation.
    uint32_t        intersect_start;
};

/*
 * The "Social" is a vector because it requires to be ordered for
 * extend_social_circles().
 *
 *  social with small circles: (sort by users.size)
 *      [ <url0> => <users...>, <url1> => <users...>, ... ]
 *
 *  social with big circles before being sorted:
 *      [ <url0,url1> => <users...>,
 *        <url0,url2> => <users...>,
 *        ...
 *        <url1,url2> => <users...>,
 *        <url1,url3> => <users...>,
 *        ...
 *      ]
 *
 *  social with big circles above after being sorted by users.size:
 *
 *      [ <url1,url3> => <users...>,
 *        ...
 *        <url0,url1> => <users...>,
 *        ...
 *        <url0,url2> => <users...>,
 *        ...
 *        <url1,url2> => <users...>,
 *        ...
 *      ]
 *
 *  social with bigger circles before being sorted:
 *      [ <url1,url3,url4> => <users...>,
 *        <url1,url3,url5> => <users...>,
 *        ...
 *        <url0,url1,url2> => <users...>,
 *        <url0,url1,url3> => <users...>,
 *        ...
 *        <url0,url2,url3> => <users...>,
 *        <url0,url2,url4> => <users...>,
 *        ...
 *        <url1,url2,url3> => <users...>,
 *        <url1,url2,url4> => <users...>,
 *        ...
 *      ]
 *
 *  The member "intersect_start" of SocialCircles is "N" of urlN in
 *  the first social.
 */
typedef vector<SocialCircle*> Social;


/**
 * A top N container which allowes equal elements, actually it's a
 * minimum heap.
 */
template<typename T, typename Greater = greater<T> >
class TopN {
public:
    typedef typename vector<T>::size_type   size_type;

    TopN(size_type n) {
        assert(n > 0);

        this->n = n;
        v.reserve(n);
        is_heap = false;
    }

    /**
     * e:           [in] the element to be pushed
     * is_popped:   [out] whether an old item is popped
     * popped_item: [out] the popped item if "popped" is true
     * return:      whether the new item is pushed successfully
     */
    bool push(const T& e, bool& popped, T& old) {
        if (v.size() < n) {
            v.push_back(e);

            popped = false;
            return true;
        }

        if (! is_heap) {
            make_heap(v.begin(), v.end(), Greater());
            is_heap = true;
        }

        T& smallest = v.front();
        if (Greater()(e, smallest)) {
            old = smallest;

            smallest = e;
            adjust_heap();

            popped = true;
            return true;
        }

        popped = false;
        return false;
    }

    void sort() {
        if (is_heap)
            sort_heap(v.begin(), v.end(), Greater());
        else
            std::sort(v.begin(), v.end(), Greater());

        is_heap = false;
    }

    const vector<T>& content(bool doSort = true) {
        if (doSort)
            sort();

        return v;
    }

    size_type capacity() const {
        return n;
    }

    size_type size() const {
        return v.size();
    }

    bool is_full() const {
        return v.size() == n;
    }

    // Get the smallest element when TopN is full
    const T& smallest_when_full() {
        assert(v.size() == n);

        if (! is_heap) {
            make_heap(v.begin(), v.end(), Greater());
            is_heap = true;
        }

        return v.front();
    }

private:
    size_type   n;
    vector<T>   v;
    bool        is_heap;

    // adjust heap from top to down
    void adjust_heap() {
        const Greater is_greater;

        const size_type start = 0;
        const T top = v[start];
        size_type i = start;        // the start node
        size_type j = 2 * i + 1;    // left child of i
        size_type k;

        while (j < n) {
            k = j + 1;              // right child of i
            if (k < n && is_greater(v[j], v[k]))
                j = k;              // j points to the smaller child

            if (is_greater(v[j], top)) {
                break;
            } else {
                v[i] = v[j];        // move smaller up
                i = j;
                j = 2 * j + 1;
            }
        }

        if (i != start)             // don't copy to itself
            v[i] = top;
    }
};


struct MoreUsers
{
    bool operator()(const SocialCircle* const& a,
                    const SocialCircle* const& b) const {
        return a->users.size() > b->users.size();
    }
};


static void push_circle_to_topN(TopN<SocialCircle*, MoreUsers>& topN,
                                SocialCircle* c)
{
    bool pushed, popped;
    SocialCircle* smaller;

    pushed = topN.push(c, popped, smaller);

    if (! pushed) {
        if (false) {
            cerr << "WARN: circle(urls=" << c->urls.size() <<
                ", users=" << c->users.size() <<
                ") is thrown due to circle count limit(" <<
                topN.capacity() << ")\n";
        }

        delete c;
    }

    if (popped) {
        c = smaller;

        if (false) {
            cerr << "WARN: circle(urls=" << c->urls.size() <<
                ", users=" << c->users.size() <<
                ") is popped due to circle count limit(" <<
                topN.capacity() << ")\n";
        }

        delete smaller;
    }
}


static void dump_social(ostream& out,
                        const Social& social,
                        Social::size_type maxCircleCount,
                        vector<UrlId>::size_type maxUrlCount,
                        vector<UserId>::size_type maxUserCount)
{
    Social::size_type circleCount = min(maxCircleCount,
            social.size());

    for (unsigned i = 0; i < circleCount; ++i) {
        const SocialCircle* c = social[i];

        vector<UrlId>::size_type urlCount =
            min(maxUrlCount, c->urls.size());
        vector<UserId>::size_type userCount =
            min(maxUserCount, c->users.size());

        out << "circle[" << i << "] of " << social.size() <<
            " circles, " << urlCount << " of " <<
            c->urls.size() << " urls:";

        for (unsigned j = 0; j < urlCount; ++j) {
            out << " " << c->urls[j];
        }
        out << "\n";

        out << "circle[" << i << "] of " << social.size() <<
            " circles, " << userCount << " of " <<
            c->users.size() << " users:";

        for (unsigned j = 0; j < userCount; ++j) {
            out << " " << c->users[j];
        }
        out << "\n\n";
    }
}


static void urls_union(vector<UrlId>& newUrls,
                       const vector<UrlId>& oldUrls,
                       const UrlId newUrl)
{
    newUrls.resize(oldUrls.size() + 1);

    vector<UrlId>::const_iterator upper =
        upper_bound(oldUrls.begin(), oldUrls.end(), newUrl);

    int n = upper - oldUrls.begin();

    copy(oldUrls.begin(), upper, newUrls.begin());
    newUrls[n] = newUrl;

    ++n;
    copy(upper, oldUrls.end(), newUrls.begin() + n);
}


static void init_first_social(const AccessLogByUrl& log,
                              Social& social,
                              unsigned maxCircleCount,
                              unsigned minUserCount)
{
    DECLARE_AUTO_CPU_TIMER(t);

    if (maxCircleCount == 0)
        return;

    TopN<SocialCircle*, MoreUsers> topN(maxCircleCount);
    vector<UserId>::size_type smallestUserCount = 0;

    AccessLogByUrl::const_iterator it = log.begin();
    for (/* empty */; it != log.end(); ++it) {
        if (it->second->size() < minUserCount)
            continue;

        if (topN.is_full()) {
            SocialCircle* const& smallest = topN.smallest_when_full();
            if (smallestUserCount < smallest->users.size())
                smallestUserCount = smallest->users.size();
        }

        if (it->second->size() <= smallestUserCount)
            continue;

        SocialCircle* c = new SocialCircle();

        c->urls.push_back(it->first);
        c->users.resize(it->second->size());
        copy(it->second->begin(), it->second->end(), c->users.begin());

        push_circle_to_topN(topN, c);
    }

    const vector<SocialCircle*>& v = topN.content();
    social.resize(v.size());
    copy(v.begin(), v.end(), social.begin());

    uint32_t size = social.size();
    for (Social::size_type i = 0; i < size; ++i) {
        social[i]->intersect_start = i + 1;
        social[i]->subCircleIndex = size;   // no sub circle
    }
}


/**
 * Fill a new social instance by extending social circles
 * in old social instance to include one more url id for
 * each circle, see the comment for "Social" type definition.
 */
static void extend_social_circles(const Social& initialSocial,
                                  Social& oldSocial,
                                  Social& newSocial,
                                  unsigned maxCircleCount,
                                  unsigned minUserCount,
                                  bool dedup = false)
{
    DECLARE_AUTO_CPU_TIMER(t);

    if (maxCircleCount == 0)
        return;

    TopN<SocialCircle*, MoreUsers> topN(maxCircleCount);
    vector<UserId>::size_type smallestUserCount = 0;

    Social::size_type initialCircleCount = initialSocial.size();
    Social::size_type oldCircleCount = oldSocial.size();
    if (oldCircleCount == 0) {
        return;
    }

    for (Social::size_type i = 0; i < oldCircleCount; ++i) {
        const SocialCircle* circleA = oldSocial[i];

        for (Social::size_type j = circleA->intersect_start;
                j < initialCircleCount; ++j) {

            // each initialCircle[j] has exactly one url
            const SocialCircle* circleB = initialSocial[j];
            assert(circleB->urls.size() == 1);

            if (topN.is_full()) {
                SocialCircle* const& smallest =
                    topN.smallest_when_full();
                if (smallestUserCount < smallest->users.size())
                    smallestUserCount = smallest->users.size();
            }

            if (circleA->users.size() < minUserCount ||
                    circleB->users.size() < minUserCount ||
                    circleA->users.size() <= smallestUserCount ||
                    circleB->users.size() <= smallestUserCount) {
                // If one circle's user count is less than the limit,
                // the user count of two circles's intersection must be
                // also less than limit.
                continue;
            }

            SocialCircle* c = new SocialCircle();

            set_intersection(circleA->users.begin(),
                             circleA->users.end(),
                             circleB->users.begin(),
                             circleB->users.end(),
                             back_inserter(c->users));

            if (c->users.size() < minUserCount ||
                    c->users.size() <= smallestUserCount) {
                delete c;
                continue;
            }

            // A specilized set_union() to merge circleA
            // and circleB(single element) into c->urls
            urls_union(c->urls, circleA->urls, circleB->urls[0]);

            c->intersect_start = circleB->intersect_start;

            // If the same users visited the bigger url set, the
            // smaller url set to same user set mapping in old social
            // is duplicated, but the smaller circle(circleA here)
            // can't be released because "c" may be thrown during
            // top N election.
            if (c->users.size() == circleA->users.size()) {
                c->subCircleIndex = i;
            } else {
                c->subCircleIndex = oldCircleCount;
            }

            push_circle_to_topN(topN, c);
        }
    }

    const vector<SocialCircle*>& v = topN.content();
    newSocial.resize(v.size());
    copy(v.begin(), v.end(), newSocial.begin());

    // Don't dedup for initialSocial because it's used for every
    // iterations of extend_social_circles().
    if (dedup && oldSocial[0]->urls.size() > 1) {
        bool found = false;

        for (Social::size_type i = 0; i < newSocial.size(); ++i) {
            uint32_t subCircleIndex = newSocial[i]->subCircleIndex;

            if (subCircleIndex < oldCircleCount) {
                found = true;
                delete oldSocial[subCircleIndex];
                oldSocial[subCircleIndex] = NULL;
            }
        }

        if (found) {
            Social::iterator end = remove(oldSocial.begin(),
                    oldSocial.end(), (SocialCircle*)NULL);
            oldSocial.resize(end - oldSocial.begin());

            if (false) {
                cerr << "Remove " << oldCircleCount - oldSocial.size()
                    << " duplicated circles\n";
            }
        }
    }
}


void read_access_log(istream&        in,
                     UserToId&       users,
                     UrlToId&        urls,
                     AccessLogByUrl& log)
{
    DECLARE_AUTO_CPU_TIMER(t);

    uint32_t users_count = 0, urls_count = 0;
    string user, url;

    user.reserve(32);
    url.reserve(256);

    while (in >> user >> url) {
        UserId user_id;
        UrlId url_id;

        if (user.empty() || url.empty())
            continue;

        /*
         * find id of this user
         */
        {
            UserToId::iterator it = users.find(user);
            if (it != users.end()) {
                user_id = it->second;
            } else {
                // check integer overflow
                if (users_count == 0 && users.size() > 0) {
                    cerr << "Too many unique users, limit is " <<
                        users.size() << "\n";
                    exit(1);
                }

                user_id = users_count;
                users[user] = user_id;
                ++users_count;
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
                // check integer overflow
                if (urls_count == 0 && urls.size() > 0) {
                    cerr << "Too many unique urls, limit is " <<
                        urls.size() << "\n";
                    exit(1);
                }

                url_id = urls_count;
                urls[url] = url_id;
                ++urls_count;
            }
        }

        /*
         * record this access: url_id => { user_id, ... }
         */
        {
            AccessLogByUrl::iterator it = log.find(url_id);
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


void cluster_social_circles(vector<Social*>& socials,
                            const AccessLogByUrl& log)
{
    DECLARE_AUTO_CPU_TIMER(t);

    Social* social = new Social();
    init_first_social(log, *social, 1000, 10);

    if (social->empty()) {
        delete social;
        cerr << "First social has zero social circle.\n";
    } else {
        Social* initialSocial = social;
        unsigned urls_count, circles_count;

        for (;;) {
            socials.push_back(social);

            // socials[i] always stores social whose circles
            // have i+1 urls
            urls_count = socials.size();
            circles_count = social->size();

            cerr << "Social: urls " << urls_count <<
                ", circles " << circles_count << "\n";

            if (false) {
                dump_social(cerr, *social, 10, 50, 20);
                cerr << "\n\n";
            }

            Social* newSocial = new Social();
            extend_social_circles(*initialSocial, *social, *newSocial,
                    circles_count, 2, true);

            social = newSocial;
            if (social->empty()) {
                delete social;
                break;
            }
        }
    }
}


int main(int argc, char** argv)
{
    (void)argc;
    (void)argv;

    DECLARE_AUTO_CPU_TIMER(t);

    UserToId users;
    UrlToId urls;
    AccessLogByUrl log;

    users.reserve(50000);
    urls.reserve(20000);

    // See http://www.drdobbs.com/cpp/184401305
    // The Standard Librarian: IOStreams and Stdio
    std::ios_base::sync_with_stdio(false);  // make iostreams very fast
    std::cin.tie(NULL);     // don't flush std::cout when read from cin

    read_access_log(cin, users, urls, log);

    cerr << "Unique users: " << users.size() <<
        ", unique urls: " << urls.size() << "\n";

    vector<Social*> socials;
    cluster_social_circles(socials, log);

    return EXIT_SUCCESS;
}

