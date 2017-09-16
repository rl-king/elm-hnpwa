const autoprefixer = require('autoprefixer');
const chalk = require('chalk');
const merge = require('webpack-merge');
const path = require('path');
const webpack = require('webpack');

const CopyWebpackPlugin = require('copy-webpack-plugin');
const ExtractTextPlugin = require('extract-text-webpack-plugin');
const HtmlWebpackPlugin = require('html-webpack-plugin');
const OptimizeCssAssetsPlugin = require('optimize-css-assets-webpack-plugin');
const WebpackNotifierPlugin = require('webpack-notifier');

const prod = 'production';
const dev = 'development';

const TARGET_ENV = process.env.npm_lifecycle_event === 'build' ? prod : dev;
const isDev = TARGET_ENV == dev;
const isProd = TARGET_ENV == prod;

const entryPath = path.join(__dirname, 'src/index.js');
const outputPath = path.join(__dirname, 'dist');
const outputFilename = isProd ? '[name]-[hash].js' : '[name].js'

console.log(chalk.yellowBright.underline.bold('Starting ' + TARGET_ENV + ' mode\n'));

var commonConfig = {
    output: {
        publicPath: '/',
        path: outputPath,
        filename: `js/${outputFilename}`,
    },
    resolve: {
        extensions: ['.js', '.elm'],
        modules: ['node_modules', path.resolve(__dirname, "src")]
    },
    module: {
        noParse: /^((?!Stylesheets).)*\.elm.*$/,
        rules: [{
            test: /\.(eot|ttf|woff|woff2|svg)$/,
            use: 'file-loader?publicPath=../../&name=Css/[hash].[ext]'
        }]
    },
    plugins: [
        new WebpackNotifierPlugin(),
        new webpack.LoaderOptionsPlugin({
            options: {
                postcss: [autoprefixer()]
            }
        }),
        new HtmlWebpackPlugin({
            template: 'src/index.html',
            filename: 'index.html',
            inject: 'body'
        })
    ]
}


if (isDev === true) {
    module.exports = merge(commonConfig, {
        entry: [
            'webpack-dev-server/client?http://localhost:5001',
            entryPath
        ],
        devServer: {
            historyApiFallback: true,
            contentBase: './src',
            hot: true,
            port: 5001,
            stats: "minimal"
        },
        module: {
            rules: [{
                test: /\.elm$/,
                exclude: [/elm-stuff/, /node_modules/, /Stylesheets\.elm$/],
                use: [{
                    loader: 'elm-hot-loader'
                },
                {
                    loader: 'elm-webpack-loader',
                    options: { debug: true }
                }]
            },
            {
                test: /\.sc?ss$/,
                use: ['style-loader', 'css-loader', 'postcss-loader', 'sass-loader']
            },
            {
                test: /Stylesheets\.elm$/,
                use: ['style-loader', 'css-loader', 'elm-css-webpack-loader']
            },]
        }
    });
}


if (isProd === true) {
    module.exports = merge(commonConfig, {
        entry: entryPath,
        module: {
            rules: [{
                test: /\.elm$/,
                exclude: [/elm-stuff/, /node_modules/],
                use: 'elm-webpack-loader'
            },
            {
                test: /\.sc?ss$/,
                use: ExtractTextPlugin.extract({
                    fallback: 'style-loader',
                    use: ['css-loader', 'postcss-loader', 'sass-loader']
                })
            },
            {
                test: /Stylesheets\.elm$/,
                use: ExtractTextPlugin.extract({
                    fallback: "style-loader",
                    use: ['css-loader', 'elm-css-webpack-loader']
                })
            },]
        },
        plugins: [
            new ExtractTextPlugin({
                filename: 'css/[name]-[hash].css',
                allChunks: true,
            }),
            new OptimizeCssAssetsPlugin({
                assetNameRegExp: /\.optimize\.css$/g,
                cssProcessor: require('cssnano'),
                cssProcessorOptions: { discardComments: { removeAll: true } },
                canPrint: true
            }),
            new CopyWebpackPlugin([
                { from: 'src', to: 'images/' }
            ]),

            new webpack.optimize.UglifyJsPlugin({
                minimize: true,
                mangle: true,
                compressor: {
                    warnings: false
                }
            })
        ]
    });
}
