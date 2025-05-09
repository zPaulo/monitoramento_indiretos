from fastapi import APIRouter
from monitoramento_indiretos.api.routes import login

api_router = APIRouter()
api_router.include_router(login.router)