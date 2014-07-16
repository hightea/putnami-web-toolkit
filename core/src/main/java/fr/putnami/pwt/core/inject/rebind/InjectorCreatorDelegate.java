package fr.putnami.pwt.core.inject.rebind;

import com.google.gwt.core.ext.GeneratorContext;
import com.google.gwt.core.ext.TreeLogger;
import com.google.gwt.user.rebind.ClassSourceFileComposerFactory;
import com.google.gwt.user.rebind.SourceWriter;

public abstract class InjectorCreatorDelegate {
	public static final int HIGHEST_PRECEDENCE = Integer.MIN_VALUE;
	public static final int LOWEST_PRECEDENCE = Integer.MAX_VALUE;

	public int getOrder() {
		return 0;
	}

	public void initComposer(ClassSourceFileComposerFactory composerFactory) {
	}

	public void writeStatic(SourceWriter srcWriter) {
	}

	public void writeFields(SourceWriter srcWriter) {
	}

	public void writeConstructor(SourceWriter srcWriter) {
	}

	public void writeMethods(SourceWriter srcWriter) {
	}

	public void writePresent(SourceWriter srcWriter) {
	}

	public void writeBeforePresent(SourceWriter srcWriter) {
	}

	public void writeAfterPresent(SourceWriter srcWriter) {
	}

	public void create(TreeLogger logger, GeneratorContext context) {
	}

}
