/**
 * This file is part of pwt.
 *
 * pwt is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * pwt is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with pwt.  If not, see <http://www.gnu.org/licenses/>.
 */
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
		// Do nothing, can be overrided
	}

	public void writeStatic(SourceWriter srcWriter) {
		// Do nothing, can be overrided
	}

	public void writeFields(SourceWriter srcWriter) {
		// Do nothing, can be overrided
	}

	public void writeConstructor(SourceWriter srcWriter) {
		// Do nothing, can be overrided
	}

	public void writeMethods(SourceWriter srcWriter) {
		// Do nothing, can be overrided
	}

	public void writePresent(SourceWriter srcWriter) {
		// Do nothing, can be overrided
	}

	public void writeBeforePresent(SourceWriter srcWriter) {
		// Do nothing, can be overrided
	}

	public void writeAfterPresent(SourceWriter srcWriter) {
		// Do nothing, can be overrided
	}

	public void create(TreeLogger logger, GeneratorContext context) {
		// Do nothing, can be overrided
	}

}
