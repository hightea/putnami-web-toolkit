package fr.putnami.pwt.core.inject.rebind.delegate;

import com.google.gwt.user.rebind.ClassSourceFileComposerFactory;
import com.google.gwt.user.rebind.SourceWriter;

import fr.putnami.pwt.core.inject.client.annotation.Secured;
import fr.putnami.pwt.core.inject.rebind.InjectorCreatorDelegate;
import fr.putnami.pwt.core.security.client.controller.SessionController;

public class InjectSecuritedCreator extends InjectorCreatorDelegate {

	private final Secured securedAnnotation;


	public InjectSecuritedCreator(Secured securedAnnotation) {
		this.securedAnnotation = securedAnnotation;
	}

	@Override
	public void initComposer(ClassSourceFileComposerFactory composerFactory) {
		composerFactory.addImport(SessionController.class.getName());
	}

	@Override
	public void writeBeforePresent(SourceWriter srcWriter) {
		if (securedAnnotation.value() != null) {
			for (String value : securedAnnotation.value()) {
				srcWriter.println("SessionController.get().checkAuthorized(\"%s\");", value);
			}
		}

	}
}
