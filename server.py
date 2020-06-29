import argparse
from pathlib import Path
import typing as t
from aiohttp import web

from protobuf import node_pb2

HERE = Path(__file__).parent

class Server:
    def __init__(self) -> None:
        self.pings = {}

    def routes(self) -> t.Iterable[web.RouteDef]:
        return [
            web.get('/', self.get_index),
            web.get('/dist/Main.js', self.get_js),
            web.post('/get_last_ping', self.get_last_ping),
            web.post('/write_ping', self.write_ping),
            # placeholder: add more route-defs here
        ]

    async def get_index(self, request: web.BaseRequest) -> web.StreamResponse:
        return web.FileResponse(HERE / 'index.html')

    async def get_js(self, request: web.BaseRequest) -> web.StreamResponse:
        return web.FileResponse(HERE / 'dist' / 'Main.js')

    async def get_last_ping(self, request: web.BaseRequest) -> web.StreamResponse:
        pb_request = node_pb2.GetLastPingRequest()
        pb_request.ParseFromString(await request.content.read())
        if self.pings:
            unix_time, answers = max(self.pings.items())
            ping = node_pb2.Ping(unix_time=unix_time, answers=answers)
            # print('returning ping:', repr(str(ping)))
            return web.Response(status=200, body=node_pb2.GetLastPingResponse(last_ping=ping).SerializeToString())
        else:
            # print('no pings')
            return web.Response(status=200, body=node_pb2.GetLastPingResponse(last_ping=None).SerializeToString())

    async def write_ping(self, request: web.BaseRequest) -> web.StreamResponse:
        pb_request = node_pb2.WritePingRequest()
        pb_request.ParseFromString(await request.content.read())
        self.pings[pb_request.ping.unix_time] = pb_request.ping.answers
        # print('wrote ping:', pb_request)
        return web.Response(status=204, body=node_pb2.WritePingResponse().SerializeToString())

parser = argparse.ArgumentParser()
parser.add_argument('--open-browser', action='store_true')
parser.add_argument('-p', '--port', type=int, default=8080)
parser.add_argument('-H', '--host', default='localhost')

if __name__ == '__main__':
    args = parser.parse_args()

    app = web.Application()
    app.add_routes(Server().routes())

    if args.open_browser:
        async def _open_browser(_):
            import webbrowser
            webbrowser.open(f'http://{args.host}:{args.port}')
        # in practice I still sometimes see Connection Refused :/
        app.on_startup.append(_open_browser)

    web.run_app(app, host=args.host, port=args.port)
