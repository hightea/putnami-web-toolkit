package fr.putnami.pwt.core.inject.rebind.delegate;

import com.google.gwt.core.client.GWT;
import com.google.gwt.core.ext.typeinfo.JField;
import com.google.gwt.core.ext.typeinfo.JType;
import com.google.gwt.user.rebind.ClassSourceFileComposerFactory;
import com.google.gwt.user.rebind.SourceWriter;

import fr.putnami.pwt.core.inject.rebind.InjectorCreatorDelegate;
import fr.putnami.pwt.core.service.client.ServiceProxy;

public class InjectServiceCreator extends InjectorCreatorDelegate {

	private final JType viewType;
	private final JField serviceField;
	private final String serviceName;
	private final String proxyTypeName;
	private boolean declareProxy = true;

	public InjectServiceCreator(JType viewType, JField serviceField) {
		this.viewType = viewType;
		this.serviceField = serviceField;

		this.serviceName = serviceField.getType().getQualifiedSourceName();

		Class fieldClass;
		try {
			fieldClass = getClass().getClassLoader().loadClass(serviceField.getType().getQualifiedBinaryName());
			if (ServiceProxy.class.isAssignableFrom(fieldClass)) {
				declareProxy = false;
				proxyTypeName = serviceField.getType().getQualifiedSourceName();
			}
			else {
				proxyTypeName = "_" + serviceField.getName() + "ServiceProxy";

			}
		}
		catch (ClassNotFoundException e) {
			throw new RuntimeException(e.getMessage(), e);
		}
	}

	@Override
	public void initComposer(ClassSourceFileComposerFactory composerFactory) {
		composerFactory.addImport(GWT.class.getName());
		composerFactory.addImport(ServiceProxy.class.getName());
		composerFactory.addImport(serviceName);

	}

	@Override
	public void writeStatic(SourceWriter srcWriter) {
		if (declareProxy) {
			srcWriter.println("interface %s extends ServiceProxy<%s_Injector, %s>, %s {}",
					proxyTypeName, viewType.getSimpleSourceName(), serviceName, serviceName);

		}
	}

	@Override
	public void writeConstructor(SourceWriter srcWriter) {
		srcWriter.println("%s = (%s) GWT.create(%s.class);", serviceField.getName(), proxyTypeName, proxyTypeName);
		srcWriter.println("((%s)%s).bindService(this);", proxyTypeName, serviceField.getName());
	}
}
