package fr.putnami.pwt.sample.rest.shared.service;

import org.fusesource.restygwt.client.DirectRestService;
import org.fusesource.restygwt.client.Options;

import javax.ws.rs.DELETE;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.PUT;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.QueryParam;

import fr.putnami.pwt.sample.rest.shared.domain.User;
import fr.putnami.pwt.sample.rest.shared.domain.UserList;

@Path("/api/users")
public interface UserService extends DirectRestService {

	@GET
	UserList listUsers(@QueryParam("page") int page);

	@Path("/{id}")
	@GET
	User getUser(@PathParam("id") long id);

	@POST
	User createUser(@QueryParam("name") String name, @QueryParam("job") String job);

	@Path("/{id}")
	@PUT
	User updateUser(@PathParam("id") long id, @QueryParam("name") String name, @QueryParam("job") String job);

	@Path("/{id}")
	@DELETE
	@Options(expect = {204})
	void deleteUser(@PathParam("id") long id);
}
