const requests = require("needle");

exports.handler = function(event, context, callback) {
    cod = event.queryStringParameters.cod
    requests.request(
        "post",
        `http://api.olhovivo.sptrans.com.br/v2.1/Login/Autenticar?token=${process.env.API_TOKEN}`,
        "",
        {json: true},
        (error, res) => {
            if (error) {
                callback(null, {
                    statusCode: 500,
                    body: "Deu ruim!"
                });
            } else {
                requests.request(
                    "get",
                    `http://api.olhovivo.sptrans.com.br/v2.1/Previsao/Parada?codigoParada=${cod}`,
                    "",
                    {json: true, cookies: res.cookies},
                    (error, res) => {
                        if (error) {
                            callback(null, {
                                statusCode: 500,
                                body: "Deu ruim depois!"
                            });
                        } else {
                            callback(null, {
                                statusCode: 200,
                                body: JSON.stringify(res.body)
                            });
                        }
                    }
                );
            }
        }
    );
};
