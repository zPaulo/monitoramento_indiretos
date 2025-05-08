import pytest
from monitoramento_indiretos.core.config import settings


endpoint = "/api/v1/auth"


@pytest.mark.asyncio
async def test_auth_sucess(monkeypatch, async_client):
    """
    Teste de autenticação de usuário com o visipec para obter token de acesso assíncrono
    """
    async def mock_post(*args, **kwargs):
        class MockResponse:
            status_code = 200
            def json(self):
                return {"AcessToken": "fake-token-123"}
        return MockResponse()
    
    monkeypatch.setattr("httpx.AsyncClient.post", mock_post)

    response = await async_client.post(endpoint, json={
        "email": "teste@exemplo.com",
        "password": "teste123"
    })

    assert response.status_code == 200

    json_response = response.json()
    assert "AcessToken" in json_response
    assert json_response["AcessToken"] == "fake-token-123"

@pytest.mark.asyncio
async def test_auth_fail(monkeypatch, async_client):
    """
    Teste autenticação com falha de usuário com o visipec
    """
    async def mock_post(*args, **kwargs):
        class MockResponse:
            status_code = 400
            def json(self):
                return {"error": "API01: User Not Found or Not Valid"}
        return MockResponse()
    
    monkeypatch.setattr("httpx.AsyncClient.post", mock_post)

        # Passando login com senha inválida
    response = await async_client.post(endpoint, json={
        "email": "teste@exemplo.com",
        "password": "teste1234"
    })

    assert response.status_code == 400
    json_response = response.json()
    assert "error" in json_response
    assert json_response["error"] == "API01: User Not Found or Not Valid"