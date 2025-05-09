from datetime import datetime, timedelta, timezone
from jose import jwt, JWTError
from fastapi import Depends, HTTPException, status
from fastapi.security import OAuth2PasswordBearer
from monitoramento_indiretos.core.config import settings

oauth2_scheme = OAuth2PasswordBearer(tokenUrl="auth/login")

def create_jwt(subject: str) -> str:
    """
    Cria um token JWT com o assunto (subject) fornecido.
    """
    expire = datetime.now(timezone.utc) + timedelta(minutes=settings.JWT_EXPIRATION)
    payload = {"sub": subject, "exp": expire}
    return jwt.encode(payload, settings.JWT_SECRET_KEY, algorithm=settings.JWT_ALGORITHM)

def decode_jwt(token: str) -> str:
    """
    Decodifica um token JWT e retorna o assunto (subject) contido nele.
    """
    try:
        payload = jwt.decode(token, settings.JWT_SECRET_KEY, algorithms=[settings.JWT_ALGORITHM])
        return payload.get("sub")
    except JWTError:
        raise HTTPException(status_code=status.HTTP_401_UNAUTHORIZED, detail="Token inválido", headers={"WWW-Authenticate": "Bearer"})