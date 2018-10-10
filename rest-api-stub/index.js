var http = require('http');
var express = require('express');
// var PORT = 9090;
var PORT = process.env.PORT || 8080;
var cors = require('cors');
var session = require('express-session');
var bodyParser = require('body-parser');

const app = express();
// app.use(express.static('public'));
app.use(cors({
    credentials: true,
    origin: true
}));
// app.use(bodyParser.json()); // support json encoded bodies
// app.use(bodyParser.urlencoded({
    // extended: true
// }));
// support encoded bodies
app.use(function (req, res, next) {
    res.header('Access-Control-Allow-Origin', 'http://localhost:4200');
    res.header('Access-Control-Allow-Methods', 'GET, POST, PUT, PATCH, DELETE');
    res.header('Access-Control-Allow-Credentials', true);

    next();
});

app.all('/language-list', function (req, res, next) {
    let response = [{
            language_id: 'en_US',
            language_name: 'English'
        },
        {
            language_id: 'it_IT',
            language_name: 'Italian'
        },
        {
            language_id: 'ga_IE',
            language_name: 'Irish'
        },
        {
            language_id: 'th_TH',
            language_name: 'Thai'
        }
    ];
    res.send(response);
});

app.all('/paradigm-list', function (req, res, next) {
    res.send([
        {
            paradigm_id: 'verb',
            paradigm_name: 'Verb'
        },
        {
            paradigm_id: 'noun',
            paradigm_name: 'Noun'
        },
        {
            paradigm_id: 'verb-ar',
            paradigm_name: 'Verb ar'
        },
        {
            paradigm_id: 'verb-xy',
            paradigm_name: 'Verb xy'
        }
    ]);
})

app.listen(PORT, () => console.log('RESTful API Stub app listening on port ' + PORT + '!'));