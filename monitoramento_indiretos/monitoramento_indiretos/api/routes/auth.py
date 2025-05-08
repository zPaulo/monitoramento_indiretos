import httpx

from fastapi import APIRouter, HTTPException
from pydantic import BaseModel

class LoginRequest(BaseModel):
    email: str
    password: str

router = APIRouter(tags=["auth"])

@router.post("/login")
async def login(data: LoginRequest):
    """
    Autenticação de usuário com o visipec para obter o token de acesso
    """
    url = "https://api.visipec.com/api/v22/authenticate-user"
    headers = {
        "Content-Type": "application/json",
        "accept": "*/*",
    }

    payload = {
        "Email": data.email,
        "Password": data.password
    }

    async with httpx.AsyncClient() as client:
        response = await client.post(url=url, json=payload, headers=headers)

    if response.status_code != 200:
        raise HTTPException(status_code=response.status_code, detail="Erro ao autenticar usuário. Verifique suas credenciais.")
    
    response_data = response.json()

    acess_token = response_data.get('AccessToken')
    if not acess_token:
        raise HTTPException(status_code=400, detail="Token de acesso não encontrado na resposta.")
    
    return {"acess_token": acess_token}