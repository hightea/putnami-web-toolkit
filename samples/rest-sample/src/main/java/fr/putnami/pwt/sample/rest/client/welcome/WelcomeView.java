package fr.putnami.pwt.sample.rest.client.welcome;

import com.google.common.collect.Lists;
import com.google.gwt.uibinder.client.UiField;
import com.google.gwt.user.client.ui.Composite;

import fr.putnami.pwt.core.inject.client.annotation.Initialize;
import fr.putnami.pwt.core.inject.client.annotation.InjectService;
import fr.putnami.pwt.core.inject.client.annotation.PostConstruct;
import fr.putnami.pwt.core.inject.client.annotation.Templated;
import fr.putnami.pwt.core.mvp.client.View;
import fr.putnami.pwt.core.service.client.annotation.AsyncHandler;
import fr.putnami.pwt.core.widget.client.TableEditor;
import fr.putnami.pwt.sample.rest.shared.domain.User;
import fr.putnami.pwt.sample.rest.shared.domain.UserList;
import fr.putnami.pwt.sample.rest.shared.service.UserService;

@Templated
public class WelcomeView extends Composite implements View {

	@InjectService
	UserService service;

	@UiField
	@Initialize
	TableEditor<User> usersTable;

	@PostConstruct
	public void postConstruct() {
		service.listUsers(0);
	}

	@AsyncHandler
	public void onListUsers(UserList result) {
		usersTable.edit(Lists.newArrayList(result.getData()));
	}
}
