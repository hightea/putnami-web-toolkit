/**
 * This file is part of pwt-code-editor.
 *
 * pwt-code-editor is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * pwt-code-editor is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with pwt-code-editor.  If not, see <http://www.gnu.org/licenses/>.
 */
package fr.putnami.pwt.plugin.code.client.base;

import java.util.Collections;
import java.util.List;

import com.google.common.collect.Iterables;
import com.google.common.collect.Lists;

import fr.putnami.pwt.core.editor.client.Error;
import fr.putnami.pwt.plugin.code.client.aspect.CodeEditorAspect;
import fr.putnami.pwt.plugin.code.client.configuration.CodeEditorConfiguration;
import fr.putnami.pwt.plugin.code.client.event.LiveValueChangeEvent;
import fr.putnami.pwt.plugin.code.client.input.CodeInput;
import fr.putnami.pwt.plugin.code.client.output.CodeOutput;

public class CodeEditorDriverImpl implements CodeEditorDriver, LiveValueChangeEvent.Handler {

	private final List<CodeEditorAspect> aspects = Lists.newArrayList();
	private final List<Error> errors = Lists.newArrayList();

	private final CodeOutput codeOutput;
	private final CodeInput codeInput;
	private String value;

	public CodeEditorDriverImpl(CodeInput codeInput, CodeOutput codeOutput) {
		this.codeInput = codeInput;
		if (this.codeInput != null) {
			this.codeInput.addLiveValueChangeHandler(this);
		}
		this.codeOutput = codeOutput;
	}

	public Iterable<Error> getErrors() {
		return this.errors == null ? Collections.<Error> emptyList() : Iterables.unmodifiableIterable(this.errors);
	}

	public void addError(Error error) {
		this.errors.add(error);
	}

	public boolean hasErrors() {
		return this.errors != null && this.errors.size() > 0;
	}

	public void addAspect(CodeEditorAspect aspect) {
		this.aspects.add(aspect);
		if (aspect.trigerOn().contains(CodeEditorAspect.AspectTrigger.INITALIZE)) {
			aspect.apply(this);
		}
	}

	public void setConfiguration(CodeEditorConfiguration configuration) {
		this.aspects.clear();
		applyConfiguration(configuration);
	}

	public void applyConfiguration(CodeEditorConfiguration configuration) {
		for (CodeEditorAspect aspect : configuration.getAspects()) {
			addAspect(aspect.copy());
		}
		if (this.value != null) {
			edit(this.value);
		}
	}

	public void edit(String object) {
		this.value = object;
		if (this.codeInput != null) {
			codeInput.setText(this.value);
		}
		for (CodeEditorAspect strategy : Iterables.filter(this.aspects, CodeEditorAspect.AspectTrigger.EDIT)) {
			strategy.apply(this);
		}
	}

	public String flush() {
		errors.clear();
		String result = value;
		for (CodeEditorAspect strategy : Iterables.filter(this.aspects, CodeEditorAspect.AspectTrigger.FLUSH)) {
			strategy.apply(this);
		}

		return result;
	}

	public void change() {
		for (CodeEditorAspect strategy : Iterables.filter(this.aspects, CodeEditorAspect.AspectTrigger.CHANGE)) {
			strategy.apply(this);
		}
	}

	public String getValue() {
		return this.value;
	}

	@Override
	public CodeInput getCodeInput() {
		return this.codeInput;
	}

	@Override
	public CodeOutput getCodeOutput() {
		return this.codeOutput;
	}

	@Override
	public void onLiveValueChange(LiveValueChangeEvent event) {
		this.value = event.getValue();
		change();
	}

	@Override
	public List<CodeEditorAspect> getAspects() {
		return this.aspects;
	}

}
