from locust import HttpLocust, TaskSet, between

def index(l):
    l.client.get("/index.html")

class UserBehavior(TaskSet):
    tasks = {index: 1}

    def on_start(self):
        index(self)


class WebsiteUser(HttpLocust):
    task_set = UserBehavior
    wait_time = between(5.0, 9.0)
