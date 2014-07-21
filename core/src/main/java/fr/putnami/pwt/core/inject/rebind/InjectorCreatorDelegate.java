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
