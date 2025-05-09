import httpx, asyncio
from monitoramento_indiretos.core.config import settings
class VisipecClient:
    def __init__(self):
        self._client = httpx.AsyncClient(base_url=settings.VISIPEC_URL_LOGIN, timeout=30)
    
    async def authemticate(self, email: str, password: str) -> str:
        """
        Autentica o usuário no Visipec e retorna o token de acesso.
        """
        r = await self._client.post(
            "/v22/authemticate-user",
            json={"Email": email, "Password": password},
            headers={"accept": "text/plain"})
        r.raise_for_status()
        return r.json()["AcessToken"]
    
    async def validate(self, token: str) -> bool:
        """
        Valida o token de acesso no Visipec.
        """
        r = await self._client.post(
            "/Jwt",
            json={"Token": token},
            headers={"accept": "text/plain"})
        
        if r.status_code == 200:
            return True
        if r.status_code == 401:
            return False
        r.raise_for_status()
        