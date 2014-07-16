package fr.putnami.pwt.core.inject.rebind.delegate;

import java.util.Collection;

import com.google.gwt.core.ext.typeinfo.JMethod;
import com.google.gwt.core.ext.typeinfo.JParameterizedType;
import com.google.gwt.core.ext.typeinfo.JType;
import com.google.gwt.place.shared.Place;
import com.google.gwt.user.rebind.ClassSourceFileComposerFactory;
import com.google.gwt.user.rebind.SourceWriter;

import fr.putnami.pwt.core.inject.rebind.InjectorCreatorDelegate;
import fr.putnami.pwt.core.mvp.client.Presenter;

public class InjectPresenterCreator extends InjectorCreatorDelegate {

	private final Collection<JMethod> presenterMethods;

	public InjectPresenterCreator(Collection<JMethod> presenterMethods) {
		this.presenterMethods = presenterMethods;
	}

	@Override
	public int getOrder() {
		return LOWEST_PRECEDENCE;
	}

	@Override
	public void writePresent(SourceWriter srcWriter) {
		for (JMethod presenterMethod : presenterMethods) {
			if (presenterMethod.getParameters().length == 0) {
				srcWriter.println("super.%s();", presenterMethod.getName());
			}
			else if (presenterMethod.getParameters().length > 0) {
				JType placeType = presenterMethod.getParameters()[0].getType();
				srcWriter.println("if(place instanceof %s){", placeType.getSimpleSourceName());
				srcWriter.indent();
				srcWriter.println("super.%s((%s) place);", presenterMethod.getName(), placeType.getSimpleSourceName());
				srcWriter.outdent();
				srcWriter.println("}");
			}
		}
	}

	@Override
	public void initComposer(ClassSourceFileComposerFactory composerFactory) {
		composerFactory.addImport(Place.class.getName());
		composerFactory.addImport(Presenter.class.getName());
		for (JMethod presenterMethod : presenterMethods) {
			if (presenterMethod.getParameters().length > 0) {
				JType placeType = presenterMethod.getParameters()[0].getType();
				composerFactory.addImport(placeType.getQualifiedSourceName());
				if (placeType instanceof JParameterizedType) {
					JParameterizedType parameterizedType = (JParameterizedType) placeType;
					for (JType paramType : parameterizedType.getTypeArgs()) {
						composerFactory.addImport(paramType.getQualifiedSourceName());
					}
				}
			}
		}

		composerFactory.addImplementedInterface(Presenter.class.getSimpleName());
	}

}
