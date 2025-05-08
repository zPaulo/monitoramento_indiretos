from fastapi import APIRouter
from monitoramento_indiretos.api.routes import auth

api_router = APIRouter()
api_router.include_router(auth.router)