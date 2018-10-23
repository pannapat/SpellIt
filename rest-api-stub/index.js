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
app.use(bodyParser.json()); // support json encoded bodies
app.use(bodyParser.urlencoded({
    extended: true
}));
// support encoded bodies
app.use(function (req, res, next) {
    res.header('Access-Control-Allow-Origin', 'http://localhost:4200');
    res.header('Access-Control-Allow-Methods', 'GET, POST, PUT, PATCH, DELETE');
    res.header('Access-Control-Allow-Credentials', true);

    next();
});

app.all('/language-list', function (req, res, next) {
    let response = LANGUGAE_LIST;
    res.send(response);
});

app.all('/add-language', function (req, res, next) {
    LANGUGAE_LIST.push(req.body);
    res.send('ok');
})

app.all('/paradigm-list', function (req, res, next) {
    res.send(PARADIGM_LIST);
})

app.all('/paradigm', function (req, res, next) {
    let paradigm_id = req.body.paradigm_name.toLocaleLowerCase().replace(" ", "-");
    // console.log(paradigm_id);
    let paradigm = {};

    if (paradigm_id === 'verb' ||
        paradigm_id === 'verb-ar' ||
        paradigm_id === 'verb-xy') {
        paradigm = PARADIGM_HASH['verb'];
    } else if (paradigm_id === 'irregular-verb') {
        paradigm = PARADIGM_HASH['irregular-verb'];
    } else if (paradigm_id === 'noun') {
        paradigm = PARADIGM_HASH['noun'];
    }

    res.send(paradigm);
})

app.all('/add-paradigm', function (req, res, next) {
    console.log(req.body);
    let paradigm_id = req.body.paradigm_name.toLowerCase().replace(" ", "-");
    PARADIGM_HASH[paradigm_id] = req.body;

    PARADIGM_LIST.push(req.body.paradigm_name);
    res.send('ok');
})

app.listen(PORT, () => console.log('RESTful API Stub app listening on port ' + PORT + '!'));

var LANGUGAE_LIST = [{
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


var PARADIGM_LIST = [
    'Verb',
    'Irregular Verb',
    'Noun',
    'Verb ar',
    'Verb xy'
];
var PARADIGM_HASH = {
    'verb': {
        paradigm_name: 'Verb',
        slots: [
            "root",
            "Present",
            "Past",
            "Participle"
        ],
        words: [{
            "root": "walk",
            "Present": "walks",
            "Past": "walked",
            "Participle": "walked"
        }, {
            "root": "jump",
            "Present": "jumps",
            "Past": "jumped",
            "Participle": "jumped"
        }]
    },
    'irregular-verb': {
        paradigm_name: 'Irregular Verb',
        slots: [
            "root",
            "Present",
            "Past",
            "Participle"
        ],
        words: [{
            "root": "go",
            "Present": "goes",
            "Past": "went",
            "Participle": "gone"
        }, {
            "root": "take",
            "Present": "takes",
            "Past": "took",
            "Participle": "taken"
        }]
    },
    'noun': {
        paradigm_name: 'Noun',
        slots: [
            "root",
            "singular",
            "plural"
        ],
        words: [{
            "root": "fox",
            "singular": "fox",
            "plural": "foxes"
        }]
    }
};

let SAMPLE_REQUEST_ADD_LANGUAGE = {
    language_id: 'eng_US',
    language_name: 'English'
}

let SAMPLE_REQUEST_ADD_PARADIGM = {
    paradigm_name: 'Noun',
    slots: [
        "root",
        "singular",
        "plural"
    ]
}