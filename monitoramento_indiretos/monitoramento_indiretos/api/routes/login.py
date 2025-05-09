import httpx
from fastapi import APIRouter, HTTPException, status
from pydantic import BaseModel, EmailStr
from monitoramento_indiretos.services.visipec import VisipecClient
from monitoramento_indiretos.services.user_token import store_token
from monitoramento_indiretos.core.security import create_jwt
import jwt  

router = APIRouter(prefix="/auth", tags=["auth"])
client = VisipecClient()

class LoginRequest(BaseModel):
    email: EmailStr
    password: str

class LoginResponse(BaseModel):
    access_token: str   # JWT próprio
    token_type: str = "bearer"

@router.post("/login", response_model=LoginResponse)
async def login(data: LoginRequest):
    """
    - Autentica no Visipec e recebe AccessToken.
    - Armazena em Redis com TTL.
    - Gera JWT curto para o front.
    """
    try:
        visipec_token = await client.authenticate(data.email, data.password)
    except httpx.HTTPStatusError as e:
        raise HTTPException(status_code=e.response.status_code,
                            detail="Credenciais Visipec inválidas.")
    
    # TTL: use 28 min (buffer) ou parse `exp` se a Visipec retornar.
    await store_token(data.email, visipec_token, ttl=28*60)
    
    jwt_token = create_jwt(data.email)
    return {"access_token": jwt_token}
