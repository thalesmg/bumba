from flask import Flask, request, send_from_directory, send_file
from flask_cors import CORS, cross_origin
import requests

app = Flask("blerp")
cors = CORS(app)
app.config["CORS_HEADERS"] = "Content-Type"

@app.route("/")
def home():
    return send_file("result/index.html")

@app.route("/public/<path:path>")
def public_static(path):
    return send_from_directory("result/public", path)

@app.route("/.netlify/functions/api")
@cross_origin()
def parada():
    print(request.args)
    s = requests.Session()
    auth = s.post("http://api.olhovivo.sptrans.com.br/v2.1/Login/Autenticar", params={"token": "2bc28344ead24b3b5fe273ec7389d2e4d14abd2bad45abe6d8d774026193f894"}, json="")
    print(auth)
    print(auth.request.headers)
    if auth.status_code != 200:
        return auth.content
    parada = request.args.get("cod")
    res = s.get("http://api.olhovivo.sptrans.com.br/v2.1/Previsao/Parada", params={"codigoParada": parada}, json="")
    print(res)
    print(res.json())
    return res.json()
