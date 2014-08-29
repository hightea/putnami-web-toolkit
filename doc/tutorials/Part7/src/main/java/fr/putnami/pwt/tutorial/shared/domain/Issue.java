package fr.putnami.pwt.tutorial.shared.domain;

import java.io.Serializable;
import java.util.List;

import javax.validation.constraints.NotNull;
import javax.validation.constraints.Size;

import com.google.common.collect.Lists;

public class Issue implements Serializable {

	private static final long serialVersionUID = 1L;

	private Long id;
	@NotNull
	@Size(min = 5, max = 50)
	private String name;
	@NotNull
	private Type type = Type.MINOR;
	private List<String> labels = Lists.newArrayList();
	@NotNull
	@Size(min = 10, max = 500)
	private String description;

	public Long getId() {
		return id;
	}

	public void setId(Long id) {
		this.id = id;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public Type getType() {
		return type;
	}

	public void setType(Type type) {
		this.type = type;
	}

	public List<String> getLabels() {
		return labels;
	}

	public void setLabels(List<String> labels) {
		this.labels = labels;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}
}
