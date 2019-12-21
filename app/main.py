from flask import Flask
from flask_cors import CORS, cross_origin
import requests

app = Flask("blerp")
cors = CORS(app)
app.config["CORS_HEADERS"] = "Content-Type"

@app.route("/api/<parada>")
@cross_origin()
def parada(parada):
    print(parada)
    s = requests.Session()
    auth = s.post("http://api.olhovivo.sptrans.com.br/v2.1/Login/Autenticar", params={"token": ""}, json="")
    print(auth)
    print(auth.request.headers)
    if auth.status_code != 200:
        return auth.content
    res = s.get("http://api.olhovivo.sptrans.com.br/v2.1/Previsao/Parada", params={"codigoParada": parada}, json="")
    print(res)
    print(res.json())
    return res.json()
