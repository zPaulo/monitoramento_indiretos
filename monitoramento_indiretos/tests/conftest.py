import pytest_asyncio
from httpx import AsyncClient, ASGITransport
from monitoramento_indiretos.main import app

@pytest_asyncio.fixture
async def async_client():
    """
    Fixture global para fornecer um cliente assíncrono para testes.
    """
    transport = ASGITransport(app=app)
    async with AsyncClient(transport=transport, base_url="http://test") as client:
        yield client
