from fastapi import FastAPI
from fastapi.routing import APIRouter
from monitoramento_indiretos.core.config import settings
from monitoramento_indiretos.api.main import api_router

app = FastAPI()

app.include_router(api_router, prefix="/api/v1", tags=["api"])