const endpoint = "https://api.hnpwa.com/v0";


module.exports = {
    cacheId: "elmhnpwa",
    filename: "service-worker.js",
    minify: true,
    navigateFallback: "/index.html",
    staticFileGlobsIgnorePatterns: [/manifest\.json$/],
    runtimeCaching: [
        {
            urlPattern: new RegExp(`${endpoint}/(news|newest|ask|show|jobs)`),
            handler: "networkFirst",
            options: {cache: {maxEntries: 30, name: "feed-cache"}}
        },
        {
            urlPattern: new RegExp(`${endpoint}/item/`),
            handler: "networkFirst",
            options: {cache: {maxEntries: 30, name: "item-cache"}}
        },
        {
            urlPattern: new RegExp(`${endpoint}/user/`),
            handler: "networkFirst",
            options: {cache: {maxEntries: 30, name: "user-cache"}}
        }
    ]
};
