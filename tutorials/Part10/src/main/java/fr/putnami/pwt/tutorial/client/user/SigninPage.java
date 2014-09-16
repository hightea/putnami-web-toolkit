package fr.putnami.pwt.tutorial.client.user;

import com.google.gwt.i18n.client.ConstantsWithLookup;
import com.google.gwt.place.shared.Place;
import com.google.gwt.uibinder.client.UiField;
import com.google.gwt.uibinder.client.UiHandler;
import com.google.gwt.user.client.ui.Composite;

import fr.putnami.pwt.core.editor.client.event.FlushSuccessEvent;
import fr.putnami.pwt.core.editor.shared.constant.ValidationConstants;
import fr.putnami.pwt.core.inject.client.annotation.Initialize;
import fr.putnami.pwt.core.inject.client.annotation.InjectResource;
import fr.putnami.pwt.core.inject.client.annotation.InjectService;
import fr.putnami.pwt.core.inject.client.annotation.PresentHandler;
import fr.putnami.pwt.core.inject.client.annotation.Templated;
import fr.putnami.pwt.core.mvp.client.MvpController;
import fr.putnami.pwt.core.mvp.client.View;
import fr.putnami.pwt.core.security.client.controller.SessionController;
import fr.putnami.pwt.core.security.shared.domain.SessionDto;
import fr.putnami.pwt.core.security.shared.domain.SigninDto;
import fr.putnami.pwt.core.security.shared.service.SessionService;
import fr.putnami.pwt.core.service.client.annotation.AsyncHandler;
import fr.putnami.pwt.core.widget.client.Alert.Type;
import fr.putnami.pwt.core.widget.client.Form;
import fr.putnami.pwt.core.widget.client.event.ButtonEvent;

@Templated
public class SigninPage extends Composite implements View {


	public interface Constants extends ConstantsWithLookup, ValidationConstants {
		@DefaultStringValue("Username (admin)")
		String username();

		@DefaultStringValue("Password (123456)")
		String password();

		@DefaultStringValue("admin")
		String usernamePlaceholder();

		@DefaultStringValue("123456")
		String passwordPlaceholder();

		@DefaultStringValue("Vos identifiant sont incorrect!")
		String messageSigninFailed();
	}

	private final SessionController sessionController = SessionController.get();

	@InjectResource
	Constants constants;

	@InjectService
	SessionService service;

	@UiField
	@Initialize(constantsClass = Constants.class)
	Form<SigninDto> signinForm;
	
	private Place fallback;

	@PresentHandler
	void onPresent(SigninPlace place) {
		this.fallback = place.getFallback();
		signinForm.edit(new SigninDto());
	}

	@UiHandler("signinForm")
	void onSubmitSignIn(FlushSuccessEvent event) {
		service.signIn(event.<SigninDto> getValue());
	}

	@AsyncHandler
	void onSignIn(SessionDto session) {
		sessionController.setSession(session);
		if (fallback != null) {
			MvpController.get().goTo(fallback);
		}
		else {
			MvpController.get().goToDefaultPlace();
		}
	}

}
